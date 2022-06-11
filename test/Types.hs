{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Fake file system types and etc.
module Types
  ( TestProcess (..)
  , TestShellState(..)
  , Entry(..)
  , EntryType(..)
  , emptyDirectory
  , defaultDirectoryPermissions
  )
where

import Control.Monad.Except
import Control.Monad.State
import Core.Types
import Data.HashMap.Strict as HashMap
import qualified Data.List
import Data.Maybe (fromMaybe)
import Data.Time (Day (ModifiedJulianDay), UTCTime (..), secondsToDiffTime)
import qualified System.Directory as Directory (Permissions (..))
import qualified System.Directory.Internal as Directory

-- | Type of entry: file or dir.
data EntryType
  = Directory [FSName]
  | File Integer UTCTime String

-- | Entry in fake file system.
data Entry = Entry
  { _name :: FSName,
    _permissions :: Directory.Permissions,
    _entryType :: EntryType
  }

-- | Shell state in fake file system.
data TestShellState = TestShellState
  { _root :: Entry,
    _operationCounter :: Integer,
    _workingPath :: FSPath,
    _currentDirectory :: Entry,
    _mapper :: HashMap FilePath Entry
  }

-- | Monad for running computations over fake file system.
newtype TestProcess a = TestProcess
  { unSt :: ExceptT FSException (State TestShellState) a
  }
  deriving (Functor, Applicative, Monad, MonadState TestShellState, MonadError FSException)

-- | Create new empty directory.
emptyDirectory :: EntryType
emptyDirectory = Directory []

-- | Create new empty file with timestamp.
emptyFile :: Integer -> EntryType
emptyFile x = File 0 (UTCTime (ModifiedJulianDay 40587) (secondsToDiffTime x)) ""

-- | Default permissions for directory.
defaultDirectoryPermissions :: Directory.Permissions
defaultDirectoryPermissions =
  Directory.Permissions
    { Directory.readable = True,
      Directory.writable = True,
      Directory.executable = False,
      Directory.searchable = True
    }

-- | Default permissions for file.
defaultFilePermissions :: Directory.Permissions
defaultFilePermissions = defaultDirectoryPermissions {Directory.searchable = False}

-- | Increases operation counter.
increaseCounter :: TestShellState -> TestShellState
increaseCounter s = s {_operationCounter = _operationCounter s + 1}

-- | Addes entry to file system.
addEntry :: EntryType -> Directory.Permissions -> FilePath -> TestProcess ()
addEntry entry perm path = do
  let name = head $ toFSPath path
  let newEntry =
        Entry
          { _name = name,
            _permissions = perm,
            _entryType = entry
          }
  let parentPath = parent path
  mapper <- gets _mapper
  updatedDirectory <- case HashMap.lookup parentPath mapper of
    Nothing -> throwError InnerFileSystemException
    Just e -> case _entryType e of
      File _ _ _ -> throwError InnerFileSystemException
      Directory list -> pure $ e {_entryType = Directory $ name : list}
  modify' (\s -> s {_mapper = insert parentPath updatedDirectory (insert path newEntry mapper)})

-- | Deletes entry from file system.
removeEntry :: FilePath -> TestProcess ()
removeEntry path = do
  let name = head $ toFSPath path
  let parentPath = parent path
  mapper <- gets _mapper
  updatedDirectory <- case HashMap.lookup parentPath mapper of
    Nothing -> throwError InnerFileSystemException
    Just entry -> case _entryType entry of
      File _ _ _ -> throwError InnerFileSystemException
      Directory list -> pure $ entry {_entryType = Directory $ Data.List.delete name list}
  modify' (\s -> s {_mapper = insert parentPath updatedDirectory (delete path mapper)})

instance FSActions TestProcess where
  throwSomeException e _ = throwError e

  getAbsolutePath path = do
    modify' increaseCounter
    current <- gets _workingPath
    pure $ current <\\> path

  fileExists path = do
    modify' increaseCounter
    mapper <- gets _mapper
    pure $ case HashMap.lookup path mapper of
      Nothing -> False
      Just entry ->
        case _entryType entry of
          Directory _ -> False
          File _ _ _ -> True

  directoryExists path = do
    modify' increaseCounter
    mapper <- gets _mapper
    pure $ case HashMap.lookup path mapper of
      Nothing -> False
      Just entry ->
        case _entryType entry of
          Directory _ -> True
          File _ _ _ -> False

  changeDirectory path = do
    modify' increaseCounter
    shell <- get
    let newPath = _workingPath shell
    dir <- case HashMap.lookup path (_mapper shell) of
      Nothing -> throwError InnerFileSystemException
      Just entry -> case _entryType entry of
        Directory _ -> pure entry
        _ -> throwError InnerFileSystemException
    modify'
      ( \s ->
          s
            { _workingPath = newPath,
              _currentDirectory = dir
            }
      )

  createDirectory path = do
    modify' increaseCounter
    addEntry emptyDirectory defaultDirectoryPermissions path

  createFile path = do
    modify' increaseCounter
    counter <- gets _operationCounter
    addEntry (emptyFile counter) defaultFilePermissions path

  showDirectory path = do
    modify' increaseCounter
    mapper <- gets _mapper
    case HashMap.lookup path mapper of
      Nothing -> throwError InnerFileSystemException
      Just entry -> case _entryType entry of
        Directory list -> pure $ FileList' list
        _ -> throwError InnerFileSystemException

  showFile path = do
    modify' increaseCounter
    mapper <- gets _mapper
    case HashMap.lookup path mapper of
      Nothing -> throwError InnerFileSystemException
      Just entry -> case _entryType entry of
        File _ _ content -> pure $ FileContent' content
        _ -> throwError InnerFileSystemException

  removeDirectory path = do
    modify' increaseCounter
    removeEntry path

  removeFile path = do
    modify' increaseCounter
    removeEntry path

  writeFile str path = do
    mapper <- gets _mapper
    newEntry <- case HashMap.lookup path mapper of
      Nothing -> throwError InnerFileSystemException
      Just entry -> case _entryType entry of
        File size timeStamp content ->
          pure $ entry
            {
              _entryType = File (fromIntegral $ 8 * length str) timeStamp str
            }
        _ -> throwError InnerFileSystemException
    modify' (\s -> s{_mapper = HashMap.insert path newEntry mapper})
