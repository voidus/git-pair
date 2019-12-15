{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import qualified Dhall
import GHC.Generics (Generic)
import qualified Options.Applicative as O
import qualified Options.Applicative.Help.Pretty as Pretty
import Paths_git_comtemplate (version)
import System.Directory
  ( XdgDirectory (XdgConfig, XdgData),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
    removeFile,
  )
import System.Exit (ExitCode (ExitFailure), die)
import System.FilePath ((</>))
import System.Process (callCommand, createProcess, shell, waitForProcess)

data Command
  = CmdReset
  | CmdAuthors [Text]
  | CmdUnsetAuthors
  | CmdStory Text
  | CmdUnsetStory
  | CmdCreateExampleAuthorsFile
  deriving (Show)

projectName :: String
projectName = "git-comtemplate"

getConfigDir :: IO FilePath
getConfigDir = do
  configDir <- getXdgDirectory XdgConfig projectName
  createDirectoryIfMissing True configDir
  return configDir

getDataDir :: IO FilePath
getDataDir = do
  dataDir <- getXdgDirectory XdgData projectName
  createDirectoryIfMissing True dataDir
  return dataDir

getTemplateFilename :: IO FilePath
getTemplateFilename =
  fmap (</> "template") getDataDir

getStateFilename :: IO FilePath
getStateFilename =
  fmap (</> "state") getDataDir

data State
  = State
      { story :: Maybe Text,
        authors :: [Author]
      }
  deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

initialState :: State
initialState = State {story = Nothing, authors = []}

exampleConfig :: Text
exampleConfig =
  T.unlines
    [ "[ { initials = \"gb\", expanded = \"Glimmer of Brightmoon <glimmer@brightmoon.com>\" }",
      ", { initials = \"c\", expanded = \"Catra <catra@the.top>\" }",
      "]"
    ]

readState :: IO State
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
            unlines
              [ "Error while reading " <> stateFilename <> ": " <> err,
                "You can remove the file to reset the state.",
                "If this error persists, please open an issue with git"
              ]
    else return initialState

writeState :: State -> IO ()
writeState state = do
  stateFilename <- getStateFilename
  Aeson.encodeFile stateFilename state

command :: O.Parser Command
command =
  let resetParser :: O.Parser Command
      resetParser = pure CmdReset
      resetInfo :: O.ParserInfo Command
      resetInfo = O.info resetParser (O.progDesc "Remove the commit message template")
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
  return
    $ Pretty.string
    $ unlines
      [ "Running each command with -h or without any arguments will show more help text.",
        "",
        "Author initials are read from \"" <> authorsFilename <> "\".",
        "It is expected to be a dhall file (https://dhall-lang.org/) containing a list of authors",
        "to what should be used in the commit message.",
        "",
        "All files managed by " <> projectName <> " are placed in " <> dataDir <> ".",
        "",
        "This is " <> projectName <> " version " <> showVersion version <> ". ",
        "",
        "In case of bugs, weirdness or great ideas, create an issue at https://github.com/voidus/git-comtemplate"
      ]

data Author
  = Author
      { initials :: Text,
        expanded :: Text
      }
  deriving (Generic, Dhall.Interpret, Aeson.FromJSON, Aeson.ToJSON)

getAuthorsFilename :: IO FilePath
getAuthorsFilename =
  fmap (</> "authors.dhall") getConfigDir

readAuthors :: IO (Map Text Author)
readAuthors = do
  authorsFilename <- getAuthorsFilename
  authorsList <- Dhall.inputFile Dhall.auto authorsFilename
  return $ Map.fromList [(initials a, a) | a <- authorsList]

main :: IO ()
main = do
  cmd <- parseCommand
  oldState <- readState
  runCommand oldState cmd

updateTemplate :: State -> IO ()
updateTemplate State {story, authors} = do
  templateFilename <- getTemplateFilename
  TIO.writeFile templateFilename contents
  where
    contents =
      case nonEmpty authors of
        Nothing -> storyLine
        Just authors' -> T.unlines $ [storyLine, ""] <> authorLines authors'
    storyLine =
      case story of
        Nothing -> ""
        Just s -> s <> ": "
    authorPrefix =
      if length authors > 1
        then "Co-authored-by: "
        else "Authored-by: "
    authorLine author = authorPrefix <> expanded author
    authorLines :: NonEmpty Author -> [Text]
    authorLines = NonEmpty.toList . fmap authorLine

setGitConfigOption :: IO ()
setGitConfigOption = do
  templateFilename <- getTemplateFilename
  callCommand $ "git config --global commit.template " <> templateFilename

unsetGitConfigOption :: IO ()
unsetGitConfigOption = do
  let commandLine = "git config --global --unset commit.template"
  (_, _, _, handle) <- createProcess (shell commandLine)
  exit <- waitForProcess handle
  case exit of
    ExitFailure code
      | code /= 5 ->
        error $ "\"" <> commandLine <> "\" returned unexpected error code " <> show code
    _ -> pure ()

runCommand :: State -> Command -> IO ()
runCommand _ CmdReset = do
  unsetGitConfigOption
  removeFile =<< getStateFilename
  removeFile =<< getTemplateFilename
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
        "I could not find the following initials in " <> authorsFilename <> ": "
          <> T.unpack (T.intercalate ", " missingInitials)
runCommand _ CmdCreateExampleAuthorsFile = do
  authorsFilename <- getAuthorsFilename
  exists <- doesFileExist authorsFilename
  if exists
    then do
      putStrLn $
        "The authors file (" <> authorsFilename <> ") already exists and \n"
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

applyState :: State -> IO ()
applyState state = do
  writeState state
  updateTemplate state
  setGitConfigOption

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just r) = Right r
maybeToEither l Nothing = Left l
