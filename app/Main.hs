module Main
  ( main,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.String qualified as String
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Version (showVersion)
import Dhall qualified
import GitPair
  ( AppState (..),
    Author (..),
    getConfigDir,
    getDataDir,
    getStateFilename,
    getTemplateFilename,
    initialState,
    projectName,
    updateTemplate,
  )
import Options.Applicative qualified as O
import Options.Applicative.Help.Pretty qualified as Pretty
import Paths_git_pair (version)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.Process (callCommand, createProcess, shell, waitForProcess)
import System.Directory (doesFileExist, removeFile)
import Data.Text.IO (hPutStrLn)
import Control.Exception (catch, throwIO, IOException)
import System.IO.Error (isDoesNotExistError)
import GHC.Show (Show(showsPrec), showString)

type Command :: Type
data Command
  = CmdReset
  | CmdConfigureGit
  | CmdAuthors [Text]
  | CmdUnsetAuthors
  | CmdStory Text
  | CmdUnsetStory
  | CmdCreateExampleAuthorsFile
  deriving stock (Show)

exampleConfig :: Text
exampleConfig =
  T.unlines
    [ "[ { initials = \"gb\", expanded = \"Glimmer of Brightmoon <glimmer@brightmoon.com>\" }",
      ", { initials = \"c\", expanded = \"Catra <catra@the.top>\" }",
      "]"
    ]

readState :: IO AppState
readState = do
  stateFilename <- getStateFilename
  exists <- doesFileExist stateFilename
  if exists
    then do
      contents <- LBS.readFile stateFilename
      case Aeson.eitherDecode' contents of
        Right state -> return state
        Left err ->
          die $
            String.unlines
              [ "Error while reading " <> stateFilename <> ": " <> err,
                "You can remove the file to reset the state.",
                "If this error persists, please open an issue with git"
              ]
    else return initialState

writeState :: AppState -> IO ()
writeState state = do
  stateFilename <- getStateFilename
  Aeson.encodeFile stateFilename state

command :: O.Parser Command
command =
  let resetParser :: O.Parser Command
      resetParser = pure CmdReset

      resetInfo :: O.ParserInfo Command
      resetInfo = O.info resetParser (O.progDesc "Remove the commit message template")

      configureGitParser :: O.Parser Command
      configureGitParser = pure CmdConfigureGit

      configureGitInfo :: O.ParserInfo Command
      configureGitInfo = O.info configureGitParser (O.progDesc "Configure git to use the template")

      unsetAuthorsParser :: O.Parser Command
      unsetAuthorsParser =
        O.flag'
          CmdUnsetAuthors
          ( O.help "Unset the authors. This will remove the (co-)authored-by part from the template"
              <> O.long "unset"
              <> O.short 'u'
          )
      setAuthorsParser :: O.Parser Command
      setAuthorsParser =
        let initialsArg =
              O.strArgument
                ( O.help "A list of initials that will be used as (co-)authors"
                    <> O.metavar "<initials>"
                )
         in CmdAuthors <$> O.some initialsArg
      authorsParser :: O.Parser Command
      authorsParser = setAuthorsParser <|> unsetAuthorsParser
      authorsInfo :: O.ParserInfo Command
      authorsInfo = O.info authorsParser (O.progDesc "Set the authors")
      unsetStoryParser :: O.Parser Command
      unsetStoryParser =
        O.flag'
          CmdUnsetStory
          ( O.help "Unset the story id"
              <> O.long "unset"
              <> O.short 'u'
          )
      setStoryParser :: O.Parser Command
      setStoryParser =
        let storyArg =
              O.strArgument
                ( O.help "The story id that will be prepended to the first line"
                    <> O.metavar "<story number>"
                )
         in CmdStory <$> storyArg
      storyParser :: O.Parser Command
      storyParser = setStoryParser <|> unsetStoryParser
      storyInfo :: O.ParserInfo Command
      storyInfo = O.info storyParser (O.progDesc "Set the story id")
      exampleAuthorsFileParser :: O.Parser Command
      exampleAuthorsFileParser =
        pure CmdCreateExampleAuthorsFile
      exampleAuthorsFileInfo :: O.ParserInfo Command
      exampleAuthorsFileInfo =
        O.info
          exampleAuthorsFileParser
          (O.progDesc "Create an example authors file (won't overwrite anything)")
   in O.hsubparser
        ( O.command "reset" resetInfo
            <> O.command "configureGit" configureGitInfo
            <> O.command "authors" authorsInfo
            <> O.command "story" storyInfo
            <> O.command "exampleAuthorsFile" exampleAuthorsFileInfo
            <> O.command "a" (O.info authorsParser $ O.progDesc "Alias for authors")
            <> O.command "s" (O.info storyParser $ O.progDesc "Alias for story")
        )

programInfo :: IO (O.InfoMod Command)
programInfo = do
  footer <- helpFooter
  return $
    O.fullDesc
      <> O.progDesc "Prepares a git commit message template"
      <> O.footerDoc (Just footer)

configParser :: IO (O.ParserInfo Command)
configParser =
  O.info (O.helper <*> command) <$> programInfo

parseCommand :: IO Command
parseCommand = do
  parser <- configParser
  let prefs =
        O.prefs
          ( O.showHelpOnEmpty
              <> O.disambiguate
          )
  O.customExecParser prefs parser

helpFooter :: IO Pretty.Doc
helpFooter = do
  dataDir <- getDataDir
  authorsFilename <- getAuthorsFilename
  return $
    Pretty.string $
      String.unlines
        [ "Running each command with -h or without any arguments will show more help text.",
          "",
          "Author initials are read from \"" <> toString authorsFilename <> "\".",
          "It is expected to be a dhall file (https://dhall-lang.org/) containing a list of authors",
          "to what should be used in the commit message.",
          "",
          "All files managed by " <> toString projectName <> " are placed in " <> dataDir <> ".",
          "",
          "This is " <> toString projectName <> " version " <> showVersion version <> ". ",
          "",
          "In case of bugs, weirdness or great ideas, create an issue at https://github.com/voidus/git-pair"
        ]

getAuthorsFilename :: IO FilePath
getAuthorsFilename =
  fmap (</> "authors.dhall") getConfigDir


type AuthorsFileNotFound :: Type
newtype AuthorsFileNotFound = AuthorsFileNotFound IOException

instance Show AuthorsFileNotFound where
  showsPrec d (AuthorsFileNotFound e) =
    showString $ String.unlines
      [ "It looks like the authors file does't exist:"
      , "    " <> show (displayException e)
      , "Maybe running `git pair exampleAuthorsFile` can help?"
      ]

instance Exception AuthorsFileNotFound


readAuthors :: IO (Map Text Author)
readAuthors = do
  authorsFilename <- getAuthorsFilename
  let mapDoesNotExist e
        | isDoesNotExistError e = throwIO $ AuthorsFileNotFound e
        | otherwise = throwIO e
  authorsList <- Dhall.inputFile Dhall.auto authorsFilename `catch` mapDoesNotExist
  return $ Map.fromList [(initials a, a) | a <- authorsList]

main :: IO ()
main = do
  cmd <- parseCommand
  oldState <- readState
  runCommand oldState cmd

setGitConfigOption :: IO ()
setGitConfigOption = do
  templateFilename <- getTemplateFilename
  callCommand $ "git config --global commit.template " <> templateFilename

unsetGitConfigOption :: IO ()
unsetGitConfigOption = do
  let commandLine = "git config --global --unset commit.template"
  (_, _, _, handle) <- createProcess (shell $ toString commandLine)
  exit <- waitForProcess handle
  case exit of
    ExitFailure code
      | code /= 5 ->
          hPutStrLn stderr $ "\"" <> commandLine <> "\" returned unexpected error code " <> show code
          -- We continue here anyway to clean up the files at least
    _ -> pure ()

runCommand :: AppState -> Command -> IO ()
runCommand _ CmdReset = do
  unsetGitConfigOption

  let ignoreDoesNotExist e
        | isDoesNotExistError e = return ()
        | otherwise = throwIO e

  (removeFile =<< getStateFilename) `catch` ignoreDoesNotExist
  (removeFile =<< getTemplateFilename) `catch` ignoreDoesNotExist
runCommand _ CmdConfigureGit = do
  setGitConfigOption
runCommand state (CmdStory story) =
  applyState $ state {story = Just story}
runCommand state CmdUnsetStory =
  applyState $ state {story = Nothing}
runCommand state (CmdAuthors selectedInitials) = do
  availableAuthors <- readAuthors
  let eitherMissingInitialOrAuthor =
        [ maybeToEither i $ Map.lookup i availableAuthors
          | i <- selectedInitials
        ]
  case partitionEithers eitherMissingInitialOrAuthor of
    ([], authors) ->
      applyState $ state {authors = authors}
    (missingInitials, _) -> do
      authorsFilename <- getAuthorsFilename
      die $
        "I could not find the following initials in "
          <> authorsFilename
          <> ": "
          <> T.unpack (T.intercalate ", " missingInitials)
runCommand _ CmdCreateExampleAuthorsFile = do
  authorsFilename <- getAuthorsFilename
  exists <- doesFileExist authorsFilename
  if exists
    then do
      putStrLn $
        "The authors file ("
          <> authorsFilename
          <> ") already exists and \n"
          <> "I'm afraid that I might break something if I overwrite it, so I won't 🐥"
      putStrLn ""
      putStrLn "If you need help with the syntax, here is what I would have put in there:"
      putStrLn ""
      TIO.putStrLn exampleConfig
    else do
      TIO.writeFile authorsFilename exampleConfig
      putStrLn $ "I wrote an example config to " <> authorsFilename <> ". I hope that helps 👽"
runCommand state CmdUnsetAuthors =
  applyState $ state {authors = []}

applyState :: AppState -> IO ()
applyState state = do
  writeState state
  updateTemplate state

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just r) = Right r
maybeToEither l Nothing = Left l
