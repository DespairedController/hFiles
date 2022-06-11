-- | Module with types for TUI.
module TUI.Types(
  TuiShellState(..)
  , Name(..)
  ) where


import Data.IORef (IORef)
import Core.Types (FSPath)
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)

-- | State of the shell for using with @Brick@.
data TuiShellState =
  TuiShellState
    {
      -- Real state. Needed for performing actions
      -- with real file system.
      realState :: IORef FSPath
      -- Cursor pointing to currently selected entry.
    , tuiStatePaths :: NonEmptyCursor FilePath
      -- Misleading name. Anything that would be shown
      -- in @Viewer@.
    , showingFile :: Maybe String
    }

-- | Named parts of TUI.
data Name
  -- There current directory content is shown.
  = CurrentDirectory
  -- Something might be dispayed in @Viewer@.
  -- (File content, exception, file/directory info, etc).
  | Viewer deriving (Eq, Ord, Show)
