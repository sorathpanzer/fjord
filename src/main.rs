use clap::{Parser, Subcommand};
use dialoguer::{theme::SimpleTheme, Confirm, Input};
use im::HashSet;
use indicatif::style::TemplateError;
use indicatif::{ProgressBar, ProgressStyle};
use std::error::Error;
use std::fmt;
use std::fs;
use std::io::{self};
use std::os::unix::fs as unix_fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

// --- Configuration Module ---
mod config {
    pub const ANSI_RESET: &str = "\x1b[0m";
    pub const DOTFILES_DIR: &str = ".config/dotfiles";
    pub const PROGRESS_TICK_RATE_MS: u64 = 100;

    pub const DEFAULT_IGNORE: &[&str] = &[".git", ".gitignore", "main.hook"];

    pub mod colors {
        pub const RED: &str = "31";
        pub const GREEN: &str = "32";
        pub const YELLOW: &str = "33";
        pub const BLUE: &str = "34";
        pub const WHITE: &str = "37";
    }

    pub mod messages {
        pub const GIT_NOT_INSTALLED: &str = "O Git não parece estar instalado ou não está no seu PATH. \nPor favor, instale o Git para continuar.";
        pub const HOME_NOT_FOUND: &str = "Não foi possível encontrar o diretório HOME.";
        pub const DOTFILES_EXISTS: &str = "⚠️ O diretório de dotfiles já existe!";
        pub const CLONE_SUCCESS: &str = "✅ Repositório clonado com sucesso!";
        pub const SYNC_SUCCESS: &str = "🎉 Sincronização concluída com sucesso!";
        pub const NO_CHANGES: &str = "✨ Sem alterações para sincronizar. Tudo atualizado!";
        pub const NO_LINKS_TO_CREATE: &str = "✨ Nenhum link novo para criar ou conflito encontrado.";
        pub const OPERATION_CANCELLED: &str = "🛑 Operação cancelada.";
        pub const COMMIT_CANCELLED: &str = "🛑 Commit cancelado. Mensagem vazia.";
        
        pub const SYMLINK_POINTS_ELSEWHERE: &str = "symlink existente aponta para outro local";
        pub const SYMLINK_READ_ERROR: &str = "erro ao ler symlink existente";
        pub const FILE_EXISTS_AT_DESTINATION: &str = "já existe um ficheiro ou diretório no local";
        
        pub const ANALYZING_DOTFILES: &str = "🔍 A analisar dotfiles e a gerar plano de ações...";
        pub const CHECKING_REPO_STATUS: &str = "🔍 A verificar o estado do repositório...";
        pub const DETECTED_CHANGES: &str = "📦 Alterações detetadas:";
        pub const AUTO_EXECUTING_ADD: &str = "🚀 A executar o comando 'add' automaticamente...";
        pub const ADDING_TO_STAGE: &str = "⚡ A adicionar ficheiros ao stage...";
        pub const PUSHING_TO_REMOTE: &str = "🚀 A enviar para o repositório remoto (push)...";
        pub const CONTACTING_REMOTE: &str = "A contactar o repositório remoto...";
        
        pub const OPERATION_REPORT: &str = "--- Relatório da Operação ---";
        pub const SUMMARY: &str = "--- Resumo ---";
        pub const LINKS_TO_CREATE: &str = "Serão criados os seguintes links:";
        
        pub const PROMPT_APPLY_CHANGES: &str = "Deseja aplicar estas alterações?";
        pub const PROMPT_COMMIT_MESSAGE: &str = "Mensagem do commit";
    }
    
    pub mod symbols {
        pub const SUCCESS: &str = "✅";
        pub const WARNING: &str = "⚠️";
        pub const ERROR: &str = "❌";
        pub const TRASH: &str = "🗑️";
        pub const ARROW: &str = "→";
    }
}

// --- Data Structures and Errors ---

#[derive(Debug, Clone, PartialEq)]
enum ConflictReason {
    SymlinkPointsElsewhere,
    FileExistsAtDestination,
    SymlinkReadError,
}

impl fmt::Display for ConflictReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SymlinkPointsElsewhere => write!(f, "{}", config::messages::SYMLINK_POINTS_ELSEWHERE),
            Self::FileExistsAtDestination => write!(f, "{}", config::messages::FILE_EXISTS_AT_DESTINATION),
            Self::SymlinkReadError => write!(f, "{}", config::messages::SYMLINK_READ_ERROR),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ResultStatus {
    Success,
    Warning,
    Error,
}

enum PlanAction {
    CreateLink { src: PathBuf, dest: PathBuf },
    SkipExists { src: PathBuf, dest: PathBuf },
    Conflict { dest: PathBuf, reason: ConflictReason },
}

struct ExecResult {
    message: String,
    status: ResultStatus,
}

impl ExecResult {
    const fn new(message: String, status: ResultStatus) -> Self {
        Self { message, status }
    }
    const fn success(message: String) -> Self {
        Self::new(message, ResultStatus::Success)
    }
    const fn warning(message: String) -> Self {
        Self::new(message, ResultStatus::Warning)
    }
    const fn error(message: String) -> Self {
        Self::new(message, ResultStatus::Error)
    }
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
        write!(f, "{} ", color("Erro:", config::colors::RED))?;
        match self {
            Self::HomeDirNotFound => write!(f, "{}", config::messages::HOME_NOT_FOUND),
            Self::GitNotInstalled => write!(f, "{}", config::messages::GIT_NOT_INSTALLED),
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

// --- Command Line Interface Definition ---

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


// --- Helper Functions ---

fn color(text: &str, code: &str) -> String {
    format!("\x1b[{code}m{text}{}", config::ANSI_RESET)
}

fn pretty(path: &Path, home_s: &str) -> String {
    path.display().to_string().replace(home_s, "~")
}

fn should_ignore(entry: &fs::DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .is_some_and(|name| config::DEFAULT_IGNORE.contains(&name))
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


// --- Core Logic: Planning and Execution ---

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

    match (dest.exists(), dest.is_symlink(), src.is_dir(), dest.is_dir()) {
        (false, _, _, _) => Box::new(std::iter::once(PlanAction::CreateLink { src, dest })),
        (true, true, _, _) => match fs::read_link(&dest) {
            Ok(target) if target.starts_with(dotfiles_dir) && target == src => {
                Box::new(std::iter::once(PlanAction::SkipExists { src, dest }))
            }
            Ok(_) => Box::new(std::iter::once(PlanAction::Conflict {
                dest,
                reason: ConflictReason::SymlinkPointsElsewhere,
            })),
            Err(_) => Box::new(std::iter::once(PlanAction::Conflict {
                dest,
                reason: ConflictReason::SymlinkReadError,
            })),
        },
        (true, false, true, true) => {
            let dir_iter = match fs::read_dir(&src) {
                Ok(iter) => Box::new(iter.flatten()) as Box<dyn Iterator<Item = fs::DirEntry>>,
                Err(e) => {
                    eprintln!("{}", color(&format!("Aviso: Não foi possível ler o diretório {}: {e}", src.display()), config::colors::YELLOW));
                    Box::new(std::iter::empty())
                }
            };
            Box::new(dir_iter.filter(|entry| !should_ignore(entry)).flat_map(move |entry| {
                create_plan_recursive(
                    entry.path(),
                    dest.join(entry.file_name()),
                    dotfiles_dir,
                    new_visited.clone(),
                )
            }))
        }
        (true, false, _, _) => Box::new(std::iter::once(PlanAction::Conflict {
            dest,
            reason: ConflictReason::FileExistsAtDestination,
        })),
    }
}

#[allow(clippy::needless_pass_by_value)]
fn unlink_recursive<'a>(
    src: PathBuf,
    dest: PathBuf,
    dotfiles_dir: &'a Path,
    home_s: &'a str,
) -> Box<dyn Iterator<Item = String> + 'a> {
    
    // We combine all conditions into a single match statement for clarity.
    match (dest.exists(), dest.is_symlink(), src.is_dir(), dest.is_dir()) {
        // Case 1: Destination is a symlink we manage. Attempt to remove it.
        (true, true, _, _) => {
            if let Ok(target) = fs::read_link(&dest) {
                if target.starts_with(dotfiles_dir) {
                    return match fs::remove_file(&dest) {
                        Ok(()) => Box::new(std::iter::once(format!(
                            "{} Link removido: {}",
                            config::symbols::TRASH,
                            pretty(&dest, home_s)
                        ))),
                        Err(e) => Box::new(std::iter::once(format!(
                            "{} Erro ao remover {}: {e}",
                            config::symbols::ERROR,
                            pretty(&dest, home_s)
                        ))),
                    };
                }
            }
            // If it's a symlink but not ours, or we can't read it, do nothing.
            Box::new(std::iter::empty())
        }

        // Case 2: Both are directories. Recurse into them.
        (true, false, true, true) => {
            let dir_iter = match fs::read_dir(&src) {
                Ok(iter) => Box::new(iter.flatten()) as Box<dyn Iterator<Item = fs::DirEntry>>,
                Err(e) => {
                    eprintln!(
                        "{}",
                        color(&format!("Aviso: Não foi possível ler o diretório {}: {e}", src.display()), config::colors::YELLOW)
                    );
                    Box::new(std::iter::empty())
                }
            };
            Box::new(
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
            )
        }

        // All other cases (destination doesn't exist, it's a file, etc.)
        // result in doing nothing.
        _ => Box::new(std::iter::empty()),
    }
}

// --- "Functional Core" Helper Functions ---

fn analyze_dotfiles(source: &Path, home: &Path) -> Result<Vec<PlanAction>, DotfilesError> {
    Ok(fs::read_dir(source)?
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
        .collect())
}

fn filter_actions_to_create(plan: &[PlanAction]) -> Vec<&PlanAction> {
    plan.iter()
        .filter(|action| matches!(action, PlanAction::CreateLink { .. }))
        .collect()
}

fn present_plan_preview(actions_to_create: &[&PlanAction], home_s: &str) {
    if actions_to_create.is_empty() {
        println!("{}", config::messages::NO_LINKS_TO_CREATE);
        return;
    }

    println!("\n{}:", config::messages::LINKS_TO_CREATE);
    for action in actions_to_create {
        if let PlanAction::CreateLink { src, dest } = action {
            println!(
                "  {} {} {}",
                color(&pretty(dest, home_s), config::colors::GREEN),
                config::symbols::ARROW,
                pretty(src, home_s)
            );
        }
    }
    println!();
}

fn get_user_confirmation(prompt: &str, non_interactive: bool) -> Result<bool, DotfilesError> {
    if non_interactive {
        return Ok(true);
    }

    Confirm::with_theme(&SimpleTheme)
        .with_prompt(color(prompt, config::colors::WHITE))
        .default(true)
        .interact()
        .map_err(DotfilesError::from)
}

fn execute_plan(plan: Vec<PlanAction>, home_s: &str) -> Vec<ExecResult> {
    plan.into_iter()
        .map(|action| match action {
            PlanAction::CreateLink { src, dest } => {
                if let Some(parent) = dest.parent() {
                    fs::create_dir_all(parent).ok();
                }
                match unix_fs::symlink(&src, &dest) {
                    Ok(()) => ExecResult::success(format!(
                        "{} Link criado: {} {} {}",
                        config::symbols::SUCCESS,
                        pretty(&dest, home_s),
                        config::symbols::ARROW,
                        pretty(&src, home_s)
                    )),
                    Err(e) => ExecResult::error(format!(
                        "{} Erro ao criar link para {}: {e}",
                        config::symbols::ERROR,
                        pretty(&dest, home_s)
                    )),
                }
            }
            PlanAction::SkipExists { src, dest } => ExecResult::warning(format!(
                "{} Link já existe: {} {} {}",
                config::symbols::WARNING,
                pretty(&dest, home_s),
                config::symbols::ARROW,
                pretty(&src, home_s)
            )),
            PlanAction::Conflict { dest, reason } => ExecResult::error(format!(
                "{} Conflito: {} ({})",
                config::symbols::ERROR,
                pretty(&dest, home_s),
                reason
            )),
        })
        .collect()
}

fn display_execution_report(results: &[ExecResult]) {
    println!("\n{}", config::messages::OPERATION_REPORT);

    let counts = results.iter().fold(
        (0_usize, 0_usize, 0_usize),
        |(created, conflicts, warnings), result| match result.status {
            ResultStatus::Success => (created.saturating_add(1), conflicts, warnings),
            ResultStatus::Error => (created, conflicts.saturating_add(1), warnings),
            ResultStatus::Warning => (created, conflicts, warnings.saturating_add(1)),
        },
    );

    for result in results {
        let color_code = match result.status {
            ResultStatus::Success => config::colors::GREEN,
            ResultStatus::Warning => config::colors::YELLOW,
            ResultStatus::Error => config::colors::RED,
        };
        println!("{}", color(&result.message, color_code));
    }

    if counts.0 > 0 || counts.1 > 0 {
        println!("{}", color(&format!("\n{}", config::messages::SUMMARY), config::colors::BLUE));
        println!("{}", color(&format!("  Links criados: {}", counts.0), config::colors::GREEN));
        println!("{}", color(&format!("  Conflitos encontrados: {}", counts.1), config::colors::RED));
        if counts.2 > 0 {
            println!("{}", color(&format!("  Avisos: {}", counts.2), config::colors::YELLOW));
        }
    }
}

fn clone_git_repository(repo_url: &str, source: &Path) -> Result<(), DotfilesError> {
    let spinner = ProgressBar::new_spinner();
    spinner.set_style(ProgressStyle::default_spinner().template("{spinner:.cyan} {msg}")?);
    spinner.set_message(config::messages::CONTACTING_REMOTE);
    spinner.enable_steady_tick(std::time::Duration::from_millis(config::PROGRESS_TICK_RATE_MS));

    let output = Command::new("git").arg("clone").arg(repo_url).arg(source.as_os_str()).stderr(Stdio::piped()).output()?;

    spinner.finish_and_clear();

    if output.status.success() {
        println!("{}", color(config::messages::CLONE_SUCCESS, config::colors::GREEN));
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(DotfilesError::GitCloneFailed(stderr.trim().to_string()))
    }
}

fn check_repository_changes(source: &Path) -> Result<bool, DotfilesError> {
    let status_output = Command::new("git").current_dir(source).arg("status").arg("--porcelain").output()?;
    if !status_output.status.success() {
        let stderr = String::from_utf8_lossy(&status_output.stderr);
        return Err(DotfilesError::GitCommandFailed(stderr.trim().to_string()));
    }
    Ok(!status_output.stdout.is_empty())
}

fn get_commit_message() -> Result<String, DotfilesError> {
    Input::with_theme(&SimpleTheme)
        .with_prompt(color(config::messages::PROMPT_COMMIT_MESSAGE, config::colors::WHITE))
        .interact_text()
        .map_err(DotfilesError::from)
}

fn perform_git_sync(source: &Path, message: &str) -> Result<(), DotfilesError> {
    println!("{}", color(config::messages::ADDING_TO_STAGE, config::colors::BLUE));
    run_git_command(source, &["add", "."], false)?;

    println!("{}", color(&format!("📝 A fazer commit com a mensagem: \"{message}\""), config::colors::BLUE));
    run_git_command(source, &["commit", "-m", message], false)?;

    println!("{}", color(config::messages::PUSHING_TO_REMOTE, config::colors::BLUE));
    run_git_command(source, &["push"], true)
}


// --- Command Functions (The "Imperative Shell") ---

fn main_add(source: &Path, home: &Path, home_s: &str, non_interactive: bool) -> Result<(), DotfilesError> {
    println!("{}", config::messages::ANALYZING_DOTFILES);
    let plan = analyze_dotfiles(source, home)?;
    let actions_to_create = filter_actions_to_create(&plan);
    
    present_plan_preview(&actions_to_create, home_s);

    if !actions_to_create.is_empty() && !get_user_confirmation(config::messages::PROMPT_APPLY_CHANGES, non_interactive)? {
             println!("  {}", config::messages::OPERATION_CANCELLED);
             return Ok(());
    }

    let results = execute_plan(plan, home_s);
    display_execution_report(&results);
    Ok(())
}

fn main_del(source: &Path, home: &Path, home_s: &str) -> Result<(), DotfilesError> {
    fs::read_dir(source)?
        .flatten()
        .filter(|entry| !should_ignore(entry))
        .flat_map(|entry| unlink_recursive(entry.path(), home.join(entry.file_name()), source, home_s))
        .for_each(|r| println!("{}", color(&r, config::colors::YELLOW)));
    Ok(())
}

fn get_dotfiles(repo: &str, source: &Path, non_interactive: bool) -> Result<(), DotfilesError> {
    if source.exists() && fs::read_dir(source)?.next().is_some() {
        println!("  {}", color(config::messages::DOTFILES_EXISTS, config::colors::YELLOW));
        return Ok(());
    }
    fs::create_dir_all(source)?;

    let repo_url = if repo.starts_with("https://") {
        repo.to_string()
    } else {
        format!("https://github.com/{repo}")
    };

    println!("📥 A clonar {repo_url}...");
    clone_git_repository(&repo_url, source)?;

    println!("\n{}", config::messages::AUTO_EXECUTING_ADD);
    let home = dirs::home_dir().ok_or(DotfilesError::HomeDirNotFound)?;
    let home_s = home.to_string_lossy();
    main_add(source, &home, &home_s, non_interactive)
}

fn main_sync(source: &Path) -> Result<(), DotfilesError> {
    if !source.is_dir() {
        return Err(DotfilesError::Msg(format!("O diretório source '{}' não foi encontrado.", source.display())));
    }

    println!("{}", config::messages::CHECKING_REPO_STATUS);
    if !check_repository_changes(source)? {
        println!("{}", color(config::messages::NO_CHANGES, config::colors::GREEN));
        return Ok(());
    }

    println!("{}", color(config::messages::DETECTED_CHANGES, config::colors::YELLOW));
    run_git_command(source, &["status", "-s"], true)?;
    println!();

    let message = get_commit_message()?;
    if message.trim().is_empty() {
        println!("  {}", config::messages::COMMIT_CANCELLED);
        return Ok(());
    }

    perform_git_sync(source, &message)?;
    println!("\n{}", color(config::messages::SYNC_SUCCESS, config::colors::GREEN));
    Ok(())
}


// --- Entry Point ---

fn run_app() -> Result<(), DotfilesError> {
    check_git_availability()?;

    let Some(home) = dirs::home_dir() else {
        return Err(DotfilesError::HomeDirNotFound);
    };

    let cli = Cli::parse();
    let home_s = home.to_string_lossy();
    let source = home.join(config::DOTFILES_DIR);

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

fn main() {
    if let Err(e) = run_app() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
