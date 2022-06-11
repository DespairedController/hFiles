module Main(
  main
  ) where

import System.Environment(getArgs)

import Control.Monad(void)

import TUI.Runner(run)
import CLI.Runner(run)

-- | Runs the file manager app.
-- There are two options: run manager with simple cli or
-- run manager with simple tui.
-- Usage : @stack run <cli | tui>@.
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then void (putStrLn usage)
    else case head args of
           "tui" -> TUI.Runner.run
           "cli" -> CLI.Runner.run
           _ -> void (putStrLn usage)
  where
    usage = "Usage: stack run <cli | tui>\nCLI supports more, TUI looks better"