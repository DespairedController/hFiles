-- | Runs CLI. Supports more than @TUI.Runner@, looks not that nice.
module CLI.Runner(
  run
  ) where

import Core.Types
    (FSCommandResult(DirectoryInfo', FileInfo', FileContent', FileList'),   FSPath, FSException, FSCommand(Exit), toFilePath, toFSPath )
import Core.CommandProcessing ( processFSCommand )
import CLI.Types ( CliProcess(unStIoRef) )
import System.Directory(getCurrentDirectory)
import ArgumentParser ( parse )
import Options.Applicative
    ( ParserResult(Failure), handleParseResult, renderFailure )
import Control.Monad.Except ()
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Data.IORef ( newIORef, readIORef, IORef )
import Control.Exception(try,IOException(..))

import System.IO ( stdout, hFlush )

-- | Runs the CLI. Initial state in current directory.
run :: IO ()
run = do
  currentDir <- getCurrentDirectory
  state <- newIORef $ toFSPath currentDir
  nextCommand state

-- | Prints line to stdout and gets next one from.
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

-- | Reads next command from stdin, parses it with
-- @ArgumentParser@ and processes in @CliProcess@.
nextCommand :: IORef FSPath -> IO()
nextCommand shell = do
  wrkng <- readIORef shell
  maybeCommand <- prompt $ toFilePath wrkng ++ "> "
  cmd <- case parse maybeCommand of
    Failure failure -> do
      (msg, _) <- pure $ renderFailure failure ""
      putStrLn msg
      hFlush stdout
      return Nothing
    result -> Just <$> handleParseResult result
  case cmd of
    Nothing -> nextCommand shell
    Just Exit -> return ()
    Just p -> do
      a <- try ((runReaderT $ unStIoRef (processFSCommand p)) shell) :: IO (Either FSException (Maybe FSCommandResult))
      case a of
        Left exception -> print exception >> hFlush stdout
        Right result ->
          case result of
            Just (FileList' list) -> putStr (unlines list) >> hFlush stdout
            Just (FileContent' content) -> putStr content >> hFlush stdout
            Just (FileInfo' info) -> print info >> hFlush stdout
            Just (DirectoryInfo' info) -> print info >> hFlush stdout
            Nothing -> pure ()
      nextCommand shell
