{-# LANGUAGE LambdaCase #-}

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Set as Set
import Options.Applicative
import System.Directory
import qualified System.Exit as Exit
import System.FilePath ((</>), takeDirectory, normalise)
import System.IO (hFlush, hPrint, hPutStrLn, stderr, stdout)
import System.Process (

  CreateProcess (..),
    callProcess,
    createProcess,
    cwd,
    proc,
    readProcessWithExitCode,
    spawnProcess,
    waitForProcess
 )

type App = ExceptT DotfilesError IO

-- --- Tipos de Dados e Erros ---

data PlanAction
  = CreateLink FilePath FilePath
  | SkipExists FilePath FilePath
  | Conflict FilePath String

newtype ExecResult = ExecResult { message :: String }

data DotfilesError
  = HomeDirNotFound
  | GitNotInstalled
  | GitCloneFailed String
  | GitCommandFailed String
  | IoError IOException
  | Msg String

instance Show DotfilesError where
  show err = color "31" "Erro: " ++
    case err of
      HomeDirNotFound      -> "Não foi possível encontrar o diretório HOME."
      GitNotInstalled      -> "O Git não parece estar instalado ou não está no seu PATH. \nPor favor, instale o Git para continuar."
      GitCloneFailed s     -> "Falha ao clonar repositório: " ++ s
      GitCommandFailed s   -> "Comando Git falhou: " ++ s
      IoError e            -> "Operação de I/O falhou: " ++ show e
      Msg s                -> s

-- --- Interface da Linha de Comandos (CLI) ---

data Commands
  = Add
  | Del
  | Sync
  | Get String
  | DefaultCmd (Maybe String)

cliParser :: Parser Commands
cliParser =
  hsubparser
    ( command "add" (info (pure Add) (progDesc "Aplica os dotfiles, criando links simbólicos"))
   <> command "del" (info (pure Del) (progDesc "Remove os links simbólicos geridos"))
   <> command "sync" (info (pure Sync) (progDesc "Sincroniza as alterações locais com o repositório remoto"))
   <> command "get" (info (Get <$> strArgument (metavar "USER/REPO")) (progDesc "Clona um repositório (ex: user/repo) e aplica os dotfiles"))
    )
    <|> DefaultCmd <$> optional (strArgument (metavar "USER/REPO" <> help "Atalho para o comando 'get'"))


data Cli = Cli
  { command' :: Commands
  , yes      :: Bool
  }

cli :: ParserInfo Cli
cli = info (helper <*> versionOption <*> programOptions)
  ( fullDesc
 <> progDesc "Um gestor de dotfiles declarativo e preguiçoso"
 <> header "dotfiles-hs - um gestor de dotfiles"
  )
  where
    versionOption = infoOption "0.1.0" (long "version" <> help "Mostrar a versão")
    programOptions = Cli
      <$> cliParser
      <*> switch (long "yes" <> short 'y' <> help "Responde 'sim' a todos os prompts de confirmação")

-- --- Funções Auxiliares ---

color :: String -> String -> String
color code text = "\x1b[" ++ code ++ "m" ++ text ++ "\x1b[0m"

pretty :: FilePath -> FilePath -> String
pretty home path = case Data.List.stripPrefix (normalise home ++ "/") (normalise path) of
  Just p  -> "~/" ++ p
  Nothing -> path

shouldIgnore :: FilePath -> Bool
shouldIgnore name = name `elem` [".", "..", ".git", ".gitignore", "main.hook"]

-- Verifica se o Git está instalado
checkGitAvailability :: App ()
checkGitAvailability = do
  result <- liftIO $ try (readProcessWithExitCode "git" ["--version"] "") :: App (Either IOException (Exit.ExitCode, String, String))
  case result of
    Right (Exit.ExitSuccess, _, _) -> return ()
    _                         -> throwE GitNotInstalled

-- Executa um comando Git
runGitCommand :: FilePath -> [String] -> Bool -> App ()
runGitCommand source args inheritStdio = do
  liftIO $ putStrLn $ "$ git " ++ unwords args
  exitCode <- liftIO $
    if inheritStdio
    then do
      ph <- spawnProcess "git" args
      waitForProcess ph
    else do
      (_, _, _, ph) <- createProcess (proc "git" args) { cwd = Just source }
      waitForProcess ph

  unless (exitCode == Exit.ExitSuccess) $
    throwE $ GitCommandFailed (unwords args)

-- --- Lógica Principal ---

createPlanRecursive :: FilePath -> FilePath -> FilePath -> Set.Set FilePath -> App [PlanAction]
createPlanRecursive src dest dotfilesDir visited =
  if Set.member src visited then return [] else do
    let newVisited = Set.insert src visited
    destExists <- liftIO $ doesPathExist dest
    if destExists
      then do
        isLink <- liftIO $ pathIsSymbolicLink dest
        if isLink
          then do
            target <- liftIO $ getSymbolicLinkTarget dest
            if normalise target == normalise src
              then return [SkipExists src dest]
              else return [Conflict dest "symlink existente aponta para outro local"]
          else do
            srcIsDir <- liftIO $ doesDirectoryExist src
            destIsDir <- liftIO $ doesDirectoryExist dest
            if srcIsDir && destIsDir
              then do
                entries <- liftIO (try (listDirectory src) :: IO (Either IOException [FilePath]))
                case entries of
                  Left e -> do
                    liftIO $ hPutStrLn stderr $ color "33" $ "Aviso: Não foi possível ler o diretório " ++ src ++ ": " ++ show e
                    return []
                  Right names -> do
                    let validNames = filter (not . shouldIgnore) names
                    concat <$> mapM (\name ->
                      createPlanRecursive (src </> name) (dest </> name) dotfilesDir newVisited
                      ) validNames
              else return [Conflict dest "já existe um ficheiro ou diretório no local"]
      else return [CreateLink src dest]

executePlan :: String -> [PlanAction] -> App [ExecResult]
executePlan homeS = mapM executeAction
  where
    executeAction (CreateLink src dest) = do
      liftIO $ createDirectoryIfMissing True (takeDirectory dest)
      result <- liftIO (try (createFileLink src dest) :: IO (Either IOException ()))
      case result of
        Right () -> return $ ExecResult $ "✅ Link criado: " ++ pretty homeS dest ++ " → " ++ pretty homeS src
        Left e   -> return $ ExecResult $ "❌ Erro ao criar link para " ++ pretty homeS dest ++ ": " ++ show e
    executeAction (SkipExists src dest) =
      return $ ExecResult $ "⚠️  Link já existe: " ++ pretty homeS dest ++ " → " ++ pretty homeS src
    executeAction (Conflict dest reason) =
      return $ ExecResult $ "❌ Conflito: " ++ pretty homeS dest ++ " (" ++ reason ++ ")"

unlinkRecursive :: FilePath -> FilePath -> FilePath -> String -> App [String]
unlinkRecursive src dest dotfilesDir homeS = do
  destExists <- liftIO $ doesPathExist dest
  if not destExists then return [] else do
    isLink <- liftIO $ pathIsSymbolicLink dest
    if isLink
      then do
        target <- liftIO $ getSymbolicLinkTarget dest
        if dotfilesDir `isPrefixOf` normalise target
          then do
            result <- liftIO (try (removeFile dest) :: IO (Either IOException ()))
            case result of
              Right () -> return ["🗑️ Link removido: " ++ pretty homeS dest]
              Left e   -> return ["❌ Erro ao remover " ++ pretty homeS dest ++ ": " ++ show e]
          else return []
      else do
        srcIsDir <- liftIO $ doesDirectoryExist src
        destIsDir <- liftIO $ doesDirectoryExist dest
        if srcIsDir && destIsDir
          then do
            entries <- liftIO (try (listDirectory src) :: IO (Either IOException [FilePath]))
            case entries of
              Left e -> do
                liftIO $ hPutStrLn stderr $ color "33" $ "Aviso: Não foi possível ler o diretório " ++ src ++ ": " ++ show e
                return []
              Right names ->
                concat <$> mapM (\name -> unlinkRecursive (src </> name) (dest </> name) dotfilesDir homeS) (filter (not . shouldIgnore) names)
          else return []

-- --- Funções de Comando ---

mainAdd :: FilePath -> FilePath -> String -> Bool -> App ()
mainAdd source home homeS nonInteractive = do
  liftIO $ putStrLn "🔍 A analisar dotfiles e a gerar plano de ações..."
  entries <- liftIO $ listDirectory source
  let validEntries = filter (not . shouldIgnore) entries
  plan <- concat <$> forM validEntries (\name ->
    createPlanRecursive (source </> name) (home </> name) source Set.empty)

  let toCreate = filter (\case (CreateLink _ _) -> True; _ -> False) plan

  if null toCreate
    then liftIO $ putStrLn "✨ Nenhum link novo para criar ou conflito encontrado."
    else do
      liftIO $ putStrLn "\nSerão criados os seguintes links:"
      forM_ toCreate $ \case
        CreateLink src dest -> liftIO $ putStrLn $ "  " ++ color "32" (pretty homeS dest) ++ " → " ++ pretty homeS src
        _ -> return ()
      liftIO $ putStrLn ""

      proceed <- if nonInteractive
        then return True
        else liftIO $ do
          putStr $ color "37" "Deseja aplicar estas alterações? (Y/n) "
          hFlush stdout
          answer <- getLine
          return $ case map toLower answer of
            ""    -> True
            "y"   -> True
            "yes" -> True
            _     -> False

      if proceed
        then do
          results <- executePlan homeS plan
          liftIO $ putStrLn "\n--- Relatório da Operação ---"
          forM_ results $ \r -> liftIO $ putStrLn $
            if "✅" `isPrefixOf` message r then color "32" (message r)
            else if "⚠️" `isPrefixOf` message r then color "33" (message r)
            else color "31" (message r)
        else liftIO $ putStrLn "🛑 Operação cancelada."

mainDel :: FilePath -> FilePath -> String -> App ()
mainDel source home homeS = do
  entries <- liftIO $ listDirectory source
  let validEntries = filter (not . shouldIgnore) entries
  results <- concat <$> forM validEntries (\name ->
    unlinkRecursive (source </> name) (home </> name) source homeS)
  liftIO $ forM_ results (putStrLn . color "33")

getDotfiles :: String -> FilePath -> Bool -> App ()
getDotfiles repoName source nonInteractive = do
  sourceExists <- liftIO $ doesDirectoryExist source
  isSourceEmpty <- if sourceExists then null <$> liftIO (listDirectory source) else return True

  if sourceExists && not isSourceEmpty
    then liftIO $ putStrLn $ color "33" "⚠️ O diretório de dotfiles já existe e não está vazio!"
    else do
      liftIO $ createDirectoryIfMissing True source
      let repoUrl = if "https://" `isPrefixOf` repoName then repoName else "https://github.com/" ++ repoName
      liftIO $ putStrLn $ "📥 A clonar " ++ repoUrl ++ "..."

      (exitCode, _, cloneStderr) <- liftIO $ readProcessWithExitCode "git" ["clone", repoUrl, source] ""

      case exitCode of
        Exit.ExitSuccess -> do
          liftIO $ putStrLn $ color "32" "✅ Repositório clonado com sucesso!"
          liftIO $ putStrLn "\n🚀 A executar o comando 'add' automaticamente..."
          home <- liftIO getHomeDirectory
          mainAdd source home home nonInteractive
        Exit.ExitFailure _ -> throwE $ GitCloneFailed cloneStderr

mainSync :: FilePath -> App ()
mainSync source = do
  sourceExists <- liftIO $ doesDirectoryExist source
  unless sourceExists $ throwE $ Msg $ "O diretório source '" ++ source ++ "' não foi encontrado."

  liftIO $ putStrLn "🔍 A verificar o estado do repositório..."
  (exitCode, gitStatusOutput, _) <- liftIO $ readProcessWithExitCode "git" ["-C", source, "status", "--porcelain"] ""

  unless (exitCode == Exit.ExitSuccess) $ throwE $ GitCommandFailed "status --porcelain"

  if null gitStatusOutput
    then liftIO $ putStrLn $ color "32" "✨ Sem alterações para sincronizar. Tudo atualizado!"
    else do
      liftIO $ putStrLn $ color "33" "📦 Alterações detetadas:"
      liftIO $ callProcess "git" ["-C", source, "status", "-s"]
      liftIO $ putStrLn ""

      message <- liftIO $ do
        putStr $ color "37" "Mensagem do commit: "
        hFlush stdout
        getLine

      if null (words message)
        then liftIO $ putStrLn "🛑 Commit cancelado. Mensagem vazia."
        else do
          runGitCommand source ["add", "."] False
          runGitCommand source ["commit", "-m", message] False
          runGitCommand source ["push"] True
          liftIO $ putStrLn $ "\n" ++ color "32" "🎉 Sincronização concluída com sucesso!"

-- --- Ponto de Entrada ---

run :: App ()
run = do
  checkGitAvailability
  cliOpts <- liftIO $ execParser cli
  home <- liftIO getHomeDirectory
  let homeS = home
  let source = home </> ".config" </> "dotfiles"

  case command' cliOpts of
    Add                  -> mainAdd source home homeS (yes cliOpts)
    Del                  -> mainDel source home homeS
    Sync                 -> mainSync source
    Get repo             -> getDotfiles repo source (yes cliOpts)
    DefaultCmd (Just r)  -> getDotfiles r source (yes cliOpts)
    DefaultCmd Nothing   -> liftIO $ putStrLn "Nenhum comando especificado. Use --help para ver as opções."

main :: IO ()
main = do
  result <- runExceptT run
  case result of
    Right () -> Exit.exitSuccess
    Left err -> do
      hPrint stderr err
      Exit.exitFailure
