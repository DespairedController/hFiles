module ArgumentParser(
  parse
  ) where

import Core.Types
import Options.Applicative

parse :: String -> ParserResult FSCommand
parse text = execParserPure defaultPrefs opts (words text)
  where
    opts =
      info
        (helper <*> parseCommand)
        (fullDesc <> header "Simple file manager" <>
         progDesc "Simple file manager for exploring file system")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseChangeDirectory :: Parser FSCommand
parseChangeDirectory = ChangeDirectory
  <$> argument str (metavar "DIRPATH" <> help "path to directory")

parseCreateDirectory :: Parser FSCommand
parseCreateDirectory = CreateDirectory
  <$> argument str (metavar "DIRNAME" <> help "directory name")

parseCreateFile :: Parser FSCommand
parseCreateFile = CreateFile
  <$> argument str (metavar "FILENAME" <> help "file name")

parseShowDirectory :: Parser FSCommand
parseShowDirectory = ShowDirectory
  <$> (optional $ argument str (metavar "DIRPATH" <> help "directory path"))

parseShowFile :: Parser FSCommand
parseShowFile = ShowFile
  <$> argument str (metavar "FILEPATH" <> help "file name")

parseRemoveDirectory :: Parser FSCommand
parseRemoveDirectory = RemoveDirectory
  <$> argument str (metavar "DIRPATH" <> help "directory name")

parseRemoveFile :: Parser FSCommand
parseRemoveFile = RemoveFile
  <$> argument str (metavar "FILEPATH" <> help "file name")

parseWriteFile :: Parser FSCommand
parseWriteFile = WriteFile
  <$> argument str (metavar "FILEPATH" <> help "file name")
  <*> argument str (metavar "TEXT" <> help "text to write")

parseInfo :: Parser FSCommand
parseInfo = Info
  <$> argument str (metavar "OBJPATH" <> help "file/directory path")

parseExit :: Parser FSCommand
parseExit = pure Exit

parseCommand :: Parser FSCommand
parseCommand = subparser $
  command "cd" (parseChangeDirectory `withInfo` "change directory to DIRPATH") <>
  command "create-folder" (parseCreateDirectory `withInfo` "create empty directory") <>
  command "create-file" (parseCreateFile `withInfo` "create empty file") <>
  command "ls" (parseShowDirectory `withInfo` "if directory path provided, shows content of given directory, else shows content of current directory ") <>
  command "cat" (parseShowFile `withInfo` "show file content") <>
  command "remove-folder" (parseRemoveDirectory `withInfo` "remove directory recursive") <>
  command "remove-file" (parseRemoveFile `withInfo` "remove file") <>
  command "write-file" (parseWriteFile `withInfo` "write text to file") <>
  command "exit" (parseExit `withInfo` "exit from file manager") <>
  command "info" (parseInfo `withInfo` "show info about file/folder")
