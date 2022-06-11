-- | Module for general command processing.
--   Provides abstraction for @FSCommand@ processing.
-- Works with any instance of @FSActions@.
module Core.CommandProcessing (
  processFSCommand
  ) where

import ArgumentParser(parse)
import System.FilePath(joinPath, splitPath, FilePath)
import Control.Monad.Reader
import Control.Monad.Except
import Data.IORef
import Control.Exception
import Control.Monad.Extra(ifM)
import Core.Types

-- | Takes four arguments:
--   * @FilePath@ to process;
--   * action to perform if path leads to file;
--   * action to perform if path leads to directory;
--   * action to perform if path doesn't exist.
processPath :: (FSActions m) => FilePath
  -> (FilePath -> m (Maybe FSCommandResult))
  -> (FilePath -> m (Maybe FSCommandResult))
  -> (FilePath -> m (Maybe FSCommandResult))
  -> m (Maybe FSCommandResult)
processPath path ifFileExists ifDirectoryExists ifNoneExists = do
  absolutePath <- getAbsolutePath path
  ifM (fileExists absolutePath)
    (ifFileExists absolutePath)
    (ifM (directoryExists absolutePath)
      (ifDirectoryExists absolutePath)
      (ifNoneExists absolutePath))

-- | Processes given @FSCommand@ according to the command type.
-- in file system @fileSystem@ which is instance of @FSActions@.
processFSCommand :: (FSActions fileSystem) => FSCommand -> fileSystem (Maybe FSCommandResult)
-- Throws @DirectoryDoesntExists@ if path leads to file or nowhere.
-- If path leads to directory, changes working directory of @fileSystem@.
processFSCommand (ChangeDirectory path) =
  processPath
    path
    (throwSomeException $ DirectoryDoesntExists path "it's a file, not directory")
    (changeDirectory >=> (\x -> pure Nothing))
    (throwSomeException  $ DirectoryDoesntExists path "directory doesn't exist")

-- Throws @CannotCreateDirectory@ if directory already exists
-- or if path leads to file. Otherwise creates directory in @fileSystem@.
processFSCommand (CreateDirectory path) =
  processPath
    path
    (throwSomeException $ CannotCreateDirectory path "file already exists")
    (throwSomeException $ CannotCreateDirectory path "directory already exists")
    (createDirectory >=> (\x -> pure Nothing))

-- Throws @CannotCreateFile@ if file already exists
-- or if path leads to directory. Otherwise creates empty file in @fileSystem@.
processFSCommand (CreateFile path) =
  processPath
    path
    (throwSomeException $ CannotCreateFile path "file already exists")
    (throwSomeException $ CannotCreateFile path "directory already exists")
    (createFile >=> (\x -> pure Nothing))

-- Throws @DirectoryDoesntExists@ if given path doesn't lead to existing directory.
-- Otherwise returns wrapped directory content.
processFSCommand (ShowDirectory maybePath) = case maybePath of
      Just path -> processPath
                    path
                    (throwSomeException $ DirectoryDoesntExists path "it's a file, not directory")
                    (showDirectory >=> (pure . Just))
                    (throwSomeException $ DirectoryDoesntExists path "directory doesn't exist")
      Nothing -> processPath
                    ""
                    (throwSomeException $ DirectoryDoesntExists "" "it's a file, not directory")
                    (showDirectory >=> (pure . Just))
                    (throwSomeException $ DirectoryDoesntExists "" "directory doesn't exist")

-- Throws @FileDoesntExist@ if given path doesn't lead to existing file.
-- Otherwise returns wrapped file content.
processFSCommand (ShowFile path) =
  processPath
    path
    (showFile >=> (pure . Just))
    (throwSomeException $ FileDoesntExist path "it's a directory, not a file")
    (throwSomeException $ FileDoesntExist path "file doesn't exist")

processFSCommand (RemoveDirectory path) =
  processPath
    path
    (throwSomeException $ CannotRemoveDirectory path "it's a file, not a directory")
    (removeDirectory >=> (\_ -> pure Nothing))
    (throwSomeException $ CannotRemoveDirectory path "directory doesn't exist")

processFSCommand (RemoveFile path) =
  processPath
  path
  (removeFile >=> (\_ -> pure Nothing))
  (throwSomeException $ CannotRemoveFile path "it's a directory, not a file")
  (throwSomeException $ CannotRemoveFile path "file doesn't exist")

processFSCommand (WriteFile path str) =
  processPath
  path
  (Core.Types.writeFile str >=> (\_ -> pure Nothing))
  (throwSomeException $ FileDoesntExist  path "it's a directory, not a file")
  (throwSomeException $ FileDoesntExist  path "file doesn't exist")

processFSCommand (Info path) =
  processPath
  path
  (getFileInfo >=> pure . Just)
  (getDirectoryInfo >=> pure . Just)
  (throwSomeException $ PathDoesntExist path "no such file or folder")

processFSCommand (Show path) =
  processPath
    path
    (showFile >=> (pure . Just))
    (\p -> do
        changeDirectory p
        res <- showDirectory p
        pure $ pure res)
    (throwSomeException $ PathDoesntExist path "no such file or folder")

processFSCommand (Remove path) =
  processPath
    path
    (\p -> do
        removeFile p
        res <- showDirectory $ parent p
        pure $ pure res)
    (\p -> do
        removeDirectory p
        res <- showDirectory $ parent p
        pure $ pure res)
    (throwSomeException $ PathDoesntExist path "no such file or folder")
