{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Real FS types.
module CLI.Types
  ( CliProcess (..),
  )
where

import Control.Exception (throwIO)
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader (ask),
    ReaderT (ReaderT),
  )
import Core.Types
  ( FSActions (..),
    FSCommandResult (..),
    FSException (..),
    FSPath,
    FileInfo(..),
    DirectoryInfo(..),
    toFSPath,
    toFilePath,
    (<//>),
    (<\\>),
  )
import Data.List ( sort, foldl' )
import Data.IORef (IORef, modifyIORef', readIORef)
import qualified System.Directory as Directory
  (getDirectoryContents, Permissions, getFileSize, getModificationTime, getPermissions,  createDirectory,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.IO as RW (readFile, writeFile)
import Control.Monad.Extra (filterM)
import Data.Time.Clock (UTCTime)
import Data.Functor((<&>))
import System.FilePath (takeExtension)

newtype CliProcess a = CliProcess
  {
    unStIoRef :: ReaderT (IORef FSPath) IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef FSPath))

modifyFilePath :: (FSPath -> FSPath) -> CliProcess ()
modifyFilePath action = do
  ioref <- ask
  liftIO $ modifyIORef' ioref action

-- | Class for file system supporting access to info about files/directories.
class (Monad m) =>  InfoAccessable m where
  getPermissions :: FilePath -> m Directory.Permissions
  getModificationTime :: FilePath -> m UTCTime
  getFileSize :: FilePath -> m Integer
  getDirectorySize :: FilePath -> m Integer
  getNumberOfFilesInside :: FilePath -> m Int

instance InfoAccessable CliProcess where
  getPermissions path = liftIO $ Directory.getPermissions path
  getModificationTime path = liftIO $ Directory.getModificationTime path
  getFileSize path = liftIO $ Directory.getFileSize path
  getNumberOfFilesInside path = liftIO $ numberOfFilesRecursive path
    where
      numberOfFilesRecursive path = do
        l <- Directory.listDirectory path
        list <- mapM (\x -> pure $ toFSPath path <\\> x) l
        files <-
          filterM Directory.doesFileExist list <&> length
        dirs <-
          filterM  Directory.doesDirectoryExist list >>=
          mapM numberOfFilesRecursive >>=
          (pure . foldl' (+) 0)
        return $ files + dirs

  getDirectorySize path = liftIO $ directorySizeRecursive path
      where
        directorySizeRecursive path = do
          l <- Directory.listDirectory path
          list <- mapM (\x -> pure $ toFSPath path <\\> x) l
          files <-
            filterM Directory.doesFileExist list >>=
            mapM Directory.getFileSize >>=
            (pure . foldl' (+) 0)
          dirs <-
            filterM Directory.doesDirectoryExist list >>=
            mapM directorySizeRecursive >>=
            (pure . foldl' (+) 0)
          return $ files + dirs

instance FSActions CliProcess where
  throwSomeException exception _ = liftIO $ throwIO exception
  changeDirectory path = modifyFilePath (\x -> toFSPath path)

  getAbsolutePath path =
    do
      ioref <- ask
      a <- liftIO $ readIORef ioref
      pure $ a <\\> path

  createDirectory path = liftIO $ Directory.createDirectory path
  createFile path = liftIO $ RW.writeFile path ""
  directoryExists path = liftIO $ Directory.doesDirectoryExist path
  fileExists path = liftIO $ Directory.doesFileExist path
  removeDirectory path = liftIO $ Directory.removeDirectoryRecursive path
  removeFile path = liftIO $ Directory.removeFile path

  showDirectory path = liftIO $ do
    list <- Directory.getDirectoryContents path
    return $ FileList' $ sort list

  showFile path = do
    str <- liftIO $ RW.readFile path
    return $ FileContent' str

  writeFile str path = liftIO $ RW.writeFile path str
  getFileInfo path = do
    permissions <- getPermissions path
    modificationTime <- getModificationTime path
    size <- getFileSize path
    return $
      FileInfo' $ FileInfo
        {
          _filePath = path
        , _fileAccessRights = permissions
        , _fileSize = size
        , _fileType = takeExtension path
        , _modificationTime = modificationTime
        }

  getDirectoryInfo path = do
    permissions <- getPermissions path
    size <- getDirectorySize path
    numberOfFilesInside <- getNumberOfFilesInside path
    return $ DirectoryInfo' $
      DirectoryInfo
        {
          _directoryPath = path
        , _directoryAccessRights = permissions
        , _directorySize = size
        , _numberOfFilesInside = numberOfFilesInside
        }
