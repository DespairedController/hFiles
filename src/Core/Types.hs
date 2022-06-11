-- | Module with core types:
-- exceptions, commands and everything else,
-- non-specific to file manager realisation.
module Core.Types(
  FSCommand(..)
  , FSException(..)
  , toFSPath
  , toFilePath
  , (<//>)
  , (<\\>)
  , combine
  , parent
  , FSActions(..)
  , FSPath
  , FSName
  , FSCommandResult(..)
  , FileInfo(..)
  , DirectoryInfo(..)
  ) where

import Control.Exception ( Exception )
import System.FilePath(splitPath, joinPath, FilePath)
import Data.Time.Clock(UTCTime)
import System.Directory(Permissions)
import qualified System.Directory as Directory

-- | Type for commands.
data FSCommand
  -- Change working directory to path.
  = ChangeDirectory FilePath
  -- Create directory on given path.
  | CreateDirectory FilePath
  -- Create file on given path.
  | CreateFile FilePath
  -- Show directory on current path (if @Nothing@),
  -- or on given path.
  | ShowDirectory (Maybe FilePath)
  -- Show file content.
  | ShowFile FilePath
  -- Shows path (merged @ShowDIrectory@ and @ShowFile@).
  | Show FilePath
  -- Delete directory.
  | RemoveDirectory FilePath
  -- Delete file.
  | RemoveFile FilePath
  -- Delete path.
  | Remove FilePath
  -- Write text to file.
  | WriteFile FilePath String
  -- Show info about file/directory.
  | Info FilePath
  -- Find by name, not supported.
  | Find FilePath FSName
  -- Close shell.
  | Exit
  deriving (Eq, Show)

-- | Information about some directory.
data DirectoryInfo
  = DirectoryInfo
    { -- Size of the directory in bytes.
      _directorySize :: Integer
      -- Absolute path to directory.
    , _directoryPath :: FilePath
      -- Number of files inside the folder.
    , _numberOfFilesInside :: Int
      -- Access rights in standart format.
    , _directoryAccessRights :: Directory.Permissions
    } deriving Eq

-- | Information about some file.
data FileInfo
  = FileInfo
    { -- Path to the file.
      _filePath :: FilePath
      -- Access rights in standart format.
    , _fileAccessRights :: Directory.Permissions
      -- Type of the file.
    , _fileType :: String
      -- Modification time.
    , _modificationTime :: UTCTime
      -- Size of the file in bytes.
    , _fileSize :: Integer
    } deriving Eq

instance Show DirectoryInfo where
  show info =
    "Directory: " ++ _directoryPath info ++ "\n" ++
    "Size(bytes): " ++ show (_directorySize info) ++ "\n" ++
    "Files: " ++ show (_numberOfFilesInside info) ++ "\n" ++
    "Permissions: " ++ showNicePermissions (_directoryAccessRights info) ++ "\n"

instance Show FileInfo where
  show info =
    "File: " ++ _filePath info ++ "\n" ++
    "File type: " ++ _fileType info ++ "\n" ++
    "Size(bytes): " ++ show (_fileSize info) ++ "\n" ++
    "Modification time: " ++ show (_modificationTime info) ++ "\n" ++
    "Permissions: " ++ showNicePermissions (_fileAccessRights info) ++ "\n"

-- | Shows @Directory.Permissios@ in nice format.
showNicePermissions :: Permissions -> String
showNicePermissions p
  = helper "r" Directory.readable ++
    helper "w" Directory.writable ++
    helper "e" Directory.executable ++
    helper "s" Directory.searchable
      where
        helper s f = s ++ (if f p then "+" else "-")

-- | List of files.
type FileList = [FilePath]

-- | File content as @String@.
type FileContent = String

-- | Result of command completion.
data FSCommandResult
  -- Information about file.
  = FileInfo' FileInfo
  -- Information about directory.
  | DirectoryInfo' DirectoryInfo
  -- Directory content.
  | FileList' FileList
  -- File content.
  | FileContent' FileContent
  deriving (Eq, Show)

-- | Type for errors.
data FSException
  = CannotCreateDirectory FSName String
  | CannotCreateFile FSName String
  | FileDoesntExist FSName String
  | DirectoryDoesntExists FSName String
  | PathDoesntExist FSName String
  | CannotRemoveDirectory FSName String
  | CannotRemoveFile FSName String
  -- Thrown if error inside FS implementation occured.
  | InnerFileSystemException
  deriving Eq

instance Show FSException where
  show (CannotCreateDirectory name msg) = "Unable to create directory " ++ name ++ "\nCause: " ++ msg
  show (CannotCreateFile name msg) = "Unable to create file " ++ name ++ "\nCause: " ++ msg
  show (FileDoesntExist name msg) = "File doesn't exist " ++ name ++ "\nCause: " ++ msg
  show (DirectoryDoesntExists name msg) = "File doesn't exist " ++ name ++ "\nCause: " ++ msg
  show (PathDoesntExist name msg) = "Path doesn't exist " ++ name ++ "\nCause: " ++ msg
  show (CannotRemoveDirectory name msg) = "Unable to remove directory " ++ name ++ "\nCause: " ++ msg
  show (CannotRemoveFile name msg) = "Unable to remove file " ++ name ++ "\nCause: " ++ msg
  show InnerFileSystemException = "Inner exception in file system occured, pls restart"
instance Exception FSException where

-- | Representation of file/folder name.
type FSName = String

-- | Representation of path.
type FSPath = [FSName]

-- | Converts @FSPath@ to @FilePath@.
toFilePath :: FSPath -> FilePath
toFilePath = joinPath . reverse

-- | Converts @FilePath@ to @FSPath@.
toFSPath :: FilePath -> FSPath
toFSPath = reverse . splitPath

-- | Shows list of files in nice format.
-- (actually not really nice).
showNice :: [FilePath] -> String
showNice = unlines

-- | Resolves given @FilePath@ relative to the
-- given @FSPath@. Returns @FSPath@.
(<//>) :: FSPath -> FilePath -> FSPath
(<//>) cur rel = combine cur (reverse (toFSPath rel))

-- | Resolves given @FilePath@ relative to the
-- given @FSPath@. Returns @FilePath@.
(<\\>) :: FSPath -> FilePath -> FilePath
p1 <\\> p2 = toFilePath (p1 <//> p2)

-- | Resolves the second path relative to the first.
combine :: FSPath -> FSPath -> FSPath
combine _ path@("/":_) = reverse path
combine path [] = path
combine path (".":rest) = combine path rest
combine (_:parent) ("..":rest) = combine parent rest
combine path (a : rest) = combine (a : path) rest

-- | Parent of given path.
parent :: FilePath -> FilePath
parent = toFilePath . tail . toFSPath

-- | Class that abstracts all actions with
-- file system. Command processing declared in
-- @Core.CommandProcessing@ will work with any instance
-- of @FSActions@.
class (Monad m) => FSActions m where
  throwSomeException :: FSException -> FilePath -> m a
  changeDirectory :: FilePath -> m ()
  getAbsolutePath :: FilePath -> m FilePath
  fileExists :: FilePath -> m Bool
  directoryExists :: FilePath -> m Bool
  createDirectory :: FilePath -> m ()
  createFile :: FilePath -> m ()
  showDirectory :: FilePath -> m FSCommandResult
  showFile :: FilePath -> m FSCommandResult
  removeDirectory :: FilePath -> m ()
  removeFile :: FilePath -> m ()
  writeFile :: String -> FilePath -> m ()
  getFileInfo :: FilePath -> m FSCommandResult
  getDirectoryInfo :: FilePath -> m FSCommandResult
