use clap::{Parser, Subcommand};
use dialoguer::{theme::SimpleTheme, Confirm, Input};
use im::HashSet;
use indicatif::style::TemplateError;
use indicatif::{ProgressBar, ProgressStyle};
use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::os::unix::fs as unix_fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

// --- Funções Auxiliares ---

fn color(text: &str, code: &str) -> String {
    format!("\x1b[{code}m{text}\x1b[0m")
}

fn pretty(path: &Path, home_s: &str) -> String {
    path.display().to_string().replace(home_s, "~")
}

fn should_ignore(entry: &fs::DirEntry) -> bool {
    const IGNORE_LIST: &[&str] = &[".git", ".gitignore", "main.hook"];
    entry
        .file_name()
        .to_str()
        .is_some_and(|name| IGNORE_LIST.contains(&name))
}

fn build_base_git_command(source: &Path, args: &[&str]) -> Command {
    let mut command = Command::new("git");
    command.current_dir(source).args(args);
    command
}

fn run_git_command(source: &Path, args: &[&str], inherit_stdio: bool) -> Result<(), DotfilesError> {
    let status = if inherit_stdio {
        build_base_git_command(source, args).status()?
    } else {
        build_base_git_command(source, args).output()?.status
    };

    if !status.success() {
        return Err(DotfilesError::GitCommandFailed(format!(
            "O comando 'git {}' falhou.",
            args.join(" ")
        )));
    }
    Ok(())
}

fn check_git_availability() -> Result<(), DotfilesError> {
    match Command::new("git").arg("--version").output() {
        Ok(_) => Ok(()),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Err(DotfilesError::GitNotInstalled),
        Err(e) => Err(DotfilesError::IoError(e)),
    }
}

// --- Estruturas de Dados e Erros ---

#[derive(Debug)]
enum PlanAction {
    CreateLink { src: PathBuf, dest: PathBuf },
    SkipExists { src: PathBuf, dest: PathBuf },
    Conflict { dest: PathBuf, reason: String },
}

struct ExecResult {
    message: String,
}

#[derive(Debug)]
enum DotfilesError {
    HomeDirNotFound,
    GitNotInstalled,
    GitCloneFailed(String),
    GitCommandFailed(String),
    IoError(io::Error),
    Msg(String),
    DialoguerError(String),
    ProgressStyleError(String),
}

impl fmt::Display for DotfilesError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", color("Erro:", "31"))?;
        match self {
            Self::HomeDirNotFound => write!(f, "Não foi possível encontrar o diretório HOME."),
            Self::GitNotInstalled => write!(f, "O Git não parece estar instalado ou não está no seu PATH. \nPor favor, instale o Git para continuar."),
            Self::GitCloneFailed(s) => write!(f, "Falha ao clonar repositório: {s}"),
            Self::GitCommandFailed(s) => write!(f, "Comando Git falhou: {s}"),
            Self::IoError(e) => write!(f, "Operação de I/O falhou: {e}"),
            Self::Msg(s) => write!(f, "{s}"),
            Self::DialoguerError(s) => write!(f, "Erro na interação com o usuário: {s}"),
            Self::ProgressStyleError(s) => write!(f, "Template de estilo inválido: {s}"),
        }
    }
}

impl Error for DotfilesError {}

impl From<io::Error> for DotfilesError {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

impl From<dialoguer::Error> for DotfilesError {
    fn from(e: dialoguer::Error) -> Self {
        Self::DialoguerError(e.to_string())
    }
}

impl From<TemplateError> for DotfilesError {
    fn from(e: TemplateError) -> Self {
        Self::ProgressStyleError(e.to_string())
    }
}

#[derive(Parser, Debug)]
#[command(version, about = "Um gestor de dotfiles declarativo e preguiçoso", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
    repo: Option<String>,
    /// Responde 'sim' a todos os prompts de confirmação
    #[arg(short, long, global = true)]
    yes: bool,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Aplica os dotfiles, criando links simbólicos
    Add,
    /// Remove os links simbólicos geridos
    Del,
    /// Sincroniza as alterações locais com o repositório remoto (add, commit, push)
    Sync,
    /// Clona um repositório (ex: user/repo) e aplica os dotfiles
    Get {
        #[arg(value_name = "USER/REPO")]
        repo: String,
    },
}

#[allow(clippy::needless_pass_by_value)]
fn create_plan_recursive<'a>(
    src: PathBuf,
    dest: PathBuf,
    dotfiles_dir: &'a Path,
    visited: HashSet<PathBuf>,
) -> Box<dyn Iterator<Item = PlanAction> + 'a> {
    if visited.contains(&src) {
        return Box::new(std::iter::empty());
    }
    let new_visited = visited.update(src.clone());
    if dest.exists() {
        if let Ok(target) = fs::read_link(&dest) {
            if target.starts_with(dotfiles_dir) && target == src {
                Box::new(std::iter::once(PlanAction::SkipExists { src, dest }))
            } else {
                Box::new(std::iter::once(PlanAction::Conflict {
                    dest,
                    reason: "symlink existente aponta para outro local".to_string(),
                }))
            }
        } else if src.is_dir() && dest.is_dir() {
            let dir_iter = match fs::read_dir(&src) {
                Ok(iter) => Box::new(iter.flatten()) as Box<dyn Iterator<Item = fs::DirEntry>>,
                Err(e) => {
                    eprintln!(
                        "{}",
                        color(
                            &format!(
                                "Aviso: Não foi possível ler o diretório {}: {e}",
                                src.display()
                            ),
                            "33"
                        )
                    );
                    Box::new(std::iter::empty())
                }
            };
            Box::new(
                dir_iter
                    .filter(|entry| !should_ignore(entry))
                    .flat_map(move |entry| {
                        create_plan_recursive(
                            entry.path(),
                            dest.join(entry.file_name()),
                            dotfiles_dir,
                            new_visited.clone(),
                        )
                    }),
            )
        } else {
            Box::new(std::iter::once(PlanAction::Conflict {
                dest,
                reason: "já existe um ficheiro ou diretório no local".to_string(),
            }))
        }
    } else {
        Box::new(std::iter::once(PlanAction::CreateLink { src, dest }))
    }
}

fn execute_plan(plan: Vec<PlanAction>, home_s: &str) -> Vec<ExecResult> {
    plan.into_iter()
        .map(|action| match action {
            PlanAction::CreateLink { src, dest } => {
                if let Some(parent) = dest.parent() {
                    fs::create_dir_all(parent).ok();
                }
                match unix_fs::symlink(&src, &dest) {
                    Ok(()) => ExecResult {
                        message: format!(
                            "✅ Link criado: {} → {}",
                            pretty(&dest, home_s),
                            pretty(&src, home_s)
                        ),
                    },
                    Err(e) => ExecResult {
                        message: format!(
                            "❌ Erro ao criar link para {}: {e}",
                            pretty(&dest, home_s)
                        ),
                    },
                }
            }
            PlanAction::SkipExists { src, dest } => ExecResult {
                message: format!(
                    "⚠️  Link já existe: {} → {}",
                    pretty(&dest, home_s),
                    pretty(&src, home_s)
                ),
            },
            PlanAction::Conflict { dest, reason } => ExecResult {
                message: format!("❌ Conflito: {} ({reason})", pretty(&dest, home_s)),
            },
        })
        .collect()
}

#[allow(clippy::needless_pass_by_value)]
fn unlink_recursive<'a>(
    src: PathBuf,
    dest: PathBuf,
    dotfiles_dir: &'a Path,
    home_s: &'a str,
) -> Box<dyn Iterator<Item = String> + 'a> {
    if !dest.exists() {
        return Box::new(std::iter::empty());
    }
    if dest.is_symlink() {
        if let Ok(target) = fs::read_link(&dest) {
            if target.starts_with(dotfiles_dir) {
                return match fs::remove_file(&dest) {
                    Ok(()) => Box::new(std::iter::once(format!(
                        "🗑️ Link removido: {}",
                        pretty(&dest, home_s)
                    ))),
                    Err(e) => Box::new(std::iter::once(format!(
                        "❌ Erro ao remover {}: {e}",
                        pretty(&dest, home_s)
                    ))),
                };
            }
        }
    } else if src.is_dir() && dest.is_dir() {
        let dir_iter = match fs::read_dir(&src) {
            Ok(iter) => Box::new(iter.flatten()) as Box<dyn Iterator<Item = fs::DirEntry>>,
            Err(e) => {
                eprintln!(
                    "{}",
                    color(
                        &format!(
                            "Aviso: Não foi possível ler o diretório {}: {e}",
                            src.display()
                        ),
                        "33"
                    )
                );
                Box::new(std::iter::empty())
            }
        };
        return Box::new(
            dir_iter
                .filter(|entry| !should_ignore(entry))
                .flat_map(move |entry| {
                    unlink_recursive(
                        entry.path(),
                        dest.join(entry.file_name()),
                        dotfiles_dir,
                        home_s,
                    )
                }),
        );
    }
    Box::new(std::iter::empty())
}
// --- Funções de Comando ---

fn main_add(
    source: &Path,
    home: &Path,
    home_s: &str,
    non_interactive: bool,
) -> Result<(), DotfilesError> {
    println!("🔍 A analisar dotfiles e a gerar plano de ações...");

    let plan: Vec<PlanAction> = fs::read_dir(source)?
        .flatten()
        .filter(|entry| !should_ignore(entry))
        .flat_map(|entry| {
            create_plan_recursive(
                entry.path(),
                home.join(entry.file_name()),
                source,
                HashSet::new(),
            )
        })
        .collect();

    let to_create: Vec<_> = plan
        .iter()
        .filter(|a| matches!(a, PlanAction::CreateLink { .. }))
        .collect();

    if to_create.is_empty() {
        println!("✨ Nenhum link novo para criar ou conflito encontrado.");
    } else {
        println!("\nSerão criados os seguintes links:");
        for action in &to_create {
            if let PlanAction::CreateLink { src, dest } = action {
                println!(
                    "  {} → {}",
                    color(&pretty(dest, home_s), "32"),
                    pretty(src, home_s)
                );
            }
        }
        println!();
        if !non_interactive
            && !Confirm::with_theme(&SimpleTheme)
                .with_prompt(color("Deseja aplicar estas alterações?", "37"))
                .default(true)
                .interact()?
        {
            println!("  🛑 Operação cancelada.");
            return Ok(());
        }
    }

    let results = execute_plan(plan, home_s);
    println!("\n--- Relatório da Operação ---");
    let created_count = results
        .iter()
        .filter(|r| r.message.starts_with("✅"))
        .count();
    let conflict_count = results
        .iter()
        .filter(|r| r.message.starts_with("❌"))
        .count();

    for r in &results {
        if r.message.starts_with("✅") {
            println!("{}", color(&r.message, "32"));
        } else if r.message.starts_with("⚠️") {
            println!("{}", color(&r.message, "33"));
        } else if r.message.starts_with("❌") {
            println!("{}", color(&r.message, "31"));
        }
    }

    if created_count > 0 || conflict_count > 0 {
        println!("{}", color("\n--- Resumo ---", "34"));
        println!(
            "{}",
            color(&format!("  Links criados: {created_count}"), "32")
        );
        println!(
            "{}",
            color(&format!("  Conflitos encontrados: {conflict_count}"), "31")
        );
    }

    Ok(())
}

fn main_del(source: &Path, home: &Path, home_s: &str) -> Result<(), DotfilesError> {
    fs::read_dir(source)?
        .flatten()
        .filter(|entry| !should_ignore(entry))
        .flat_map(|entry| {
            unlink_recursive(entry.path(), home.join(entry.file_name()), source, home_s)
        })
        .for_each(|r| println!("{}", color(&r, "33")));
    Ok(())
}

fn get_dotfiles(repo: &str, source: &Path, non_interactive: bool) -> Result<(), DotfilesError> {
    if source.exists() && fs::read_dir(source)?.next().is_some() {
        println!("{}", color("  ⚠️ O diretório de dotfiles já existe!", "33"));
        return Ok(());
    }
    fs::create_dir_all(source)?;

    let repo_url = if repo.starts_with("https://") {
        repo.to_string()
    } else {
        format!("https://github.com/{repo}")
    };

    println!("📥 A clonar {repo_url}...");

    let spinner = ProgressBar::new_spinner();
    spinner.set_style(ProgressStyle::default_spinner().template("{spinner:.cyan} {msg}")?);
    spinner.set_message("A contactar o repositório remoto...");
    spinner.enable_steady_tick(std::time::Duration::from_millis(100));

    let output = Command::new("git")
        .arg("clone")
        .arg(&repo_url)
        .arg(source.as_os_str())
        .stderr(Stdio::piped())
        .output()?;

    spinner.finish_and_clear();

    if output.status.success() {
        println!("{}", color("✅ Repositório clonado com sucesso!", "32"));
        println!("\n🚀 A executar o comando 'add' automaticamente...");
        let home = dirs::home_dir().ok_or(DotfilesError::HomeDirNotFound)?;
        let home_s = home.to_string_lossy();
        main_add(source, &home, &home_s, non_interactive)?;
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(DotfilesError::GitCloneFailed(stderr.trim().to_string()));
    }
    Ok(())
}

fn main_sync(source: &Path) -> Result<(), DotfilesError> {
    if !source.is_dir() {
        return Err(DotfilesError::Msg(format!(
            "O diretório source '{}' não foi encontrado.",
            source.display()
        )));
    }

    println!("🔍 A verificar o estado do repositório...");

    let status_output = Command::new("git")
        .current_dir(source)
        .arg("status")
        .arg("--porcelain")
        .output()?;
    if !status_output.status.success() {
        let stderr = String::from_utf8_lossy(&status_output.stderr);
        return Err(DotfilesError::GitCommandFailed(stderr.trim().to_string()));
    }

    if status_output.stdout.is_empty() {
        println!(
            "{}",
            color("✨ Sem alterações para sincronizar. Tudo atualizado!", "32")
        );
        return Ok(());
    }

    println!("{}", color("📦 Alterações detetadas:", "33"));
    run_git_command(source, &["status", "-s"], true)?;
    println!();

    let message: String = Input::with_theme(&SimpleTheme)
        .with_prompt(color("Mensagem do commit", "37"))
        .interact_text()?;

    if message.trim().is_empty() {
        println!("  🛑 Commit cancelado. Mensagem vazia.");
        return Ok(());
    }

    println!("{}", color("⚡ A adicionar ficheiros ao stage...", "34"));
    run_git_command(source, &["add", "."], false)?;

    println!(
        "{}",
        color(
            &format!("📝 A fazer commit com a mensagem: \"{message}\""),
            "34"
        )
    );
    run_git_command(source, &["commit", "-m", &message], false)?;

    println!(
        "{}",
        color("🚀 A enviar para o repositório remoto (push)...", "34")
    );
    run_git_command(source, &["push"], true)?;

    println!(
        "\n{}",
        color("🎉 Sincronização concluída com sucesso!", "32")
    );

    Ok(())
}

// --- Ponto de Entrada ---

fn main() {
    if let Err(e) = run_app() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}

// Todo o código que estava em `main` e na closure `run` passa para aqui.
fn run_app() -> Result<(), DotfilesError> {
    check_git_availability()?;

    let Some(home) = dirs::home_dir() else {
        return Err(DotfilesError::HomeDirNotFound);
    };

    let cli = Cli::parse();
    let home_s = home.to_string_lossy();
    let source = home.join(".config/dotfiles");

    match cli.command {
        Some(Commands::Add) => main_add(&source, &home, &home_s, cli.yes),
        Some(Commands::Del) => main_del(&source, &home, &home_s),
        Some(Commands::Sync) => main_sync(&source),
        Some(Commands::Get { repo }) => get_dotfiles(&repo, &source, cli.yes),
        None => cli.repo.map_or_else(
            || {
                Cli::parse_from(vec!["", "--help"]);
                Ok(())
            },
            |repo| get_dotfiles(&repo, &source, cli.yes),
        ),
    }
}
