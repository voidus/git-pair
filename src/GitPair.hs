{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module GitPair where

import Dhall (FromDhall)
import qualified Data.Aeson as Aeson
import System.FilePath ((</>))
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig, XdgData), createDirectoryIfMissing)
import qualified Data.Text.IO as TextIO

projectName :: Text
projectName = "git-pair"

------------------
--- Base types ---
------------------

type Author :: Type
data Author
  = Author
      { initials :: Text,
        expanded :: Text
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromDhall, Aeson.FromJSON, Aeson.ToJSON)

type AppState :: Type
data AppState
  = AppState
      { story :: Maybe Text,
        authors :: [Author]
      }
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

initialState :: AppState
initialState = AppState {story = Nothing, authors = []}

-------------
--- Paths ---
-------------

getConfigDir :: IO FilePath
getConfigDir = do
  configDir <- getXdgDirectory XdgConfig $ toString projectName
  createDirectoryIfMissing True configDir
  return configDir

getDataDir :: IO FilePath
getDataDir = do
  dataDir <- getXdgDirectory XdgData $ toString projectName
  createDirectoryIfMissing True dataDir
  return dataDir

getTemplateFilename :: IO FilePath
getTemplateFilename =
  fmap (</> "template") getDataDir

getStateFilename :: IO FilePath
getStateFilename =
  fmap (</> "state") getDataDir
----------------------
--- Template stuff ---
----------------------

updateTemplate :: AppState -> IO ()
updateTemplate state = do
  templateFilename <- getTemplateFilename
  TextIO.writeFile templateFilename $ templateContents state

templateContents :: AppState -> Text
templateContents AppState {..}
    | length authors >= 2 =
        unlines $ [storyMarker, "", ""] <> authorLines
    | otherwise =
        storyMarker
    where
        storyMarker =
            case story of
                Nothing -> ""
                Just s -> "#" <> s
        authorLines :: [Text]
        authorLines =
            authors
            & fmap (\a -> "Co-authored-by: " <> expanded a)
            & sort

