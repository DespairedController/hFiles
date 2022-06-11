module FileSystemSpec
  (
    spec_FileCreationTest
  , spec_FileDeletionTest
  , spec_DirectoryCreationTest
  , spec_DirectoryDeletionTest
  , spec_WritingToFile
  ) where

import Test.Tasty.Hspec
import qualified Data.HashMap.Strict as HashMap
import Types
import Core.Types (FSException(..),  FSCommandResult(..),  FSCommand(..), FSActions(createFile))
import Control.Monad.State (evalState, runState)

import Core.CommandProcessing
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (StateT(runStateT), MonadState(put), State)
-- | Default root folder for test.
defaultRoot :: Entry
defaultRoot = Entry {
                      _name = "/"
                    , _permissions = defaultDirectoryPermissions
                    , _entryType = emptyDirectory
                    }

-- | Initial state of the test file system.
initialState :: TestShellState
initialState
  = TestShellState
    {
      _workingPath = ["/"]
    , _root = defaultRoot
    , _mapper = HashMap.insert "/" defaultRoot HashMap.empty
    , _operationCounter = 0
    , _currentDirectory = defaultRoot
    }

-- | Run action on initial state.
runOnInitial :: TestProcess (Maybe FSCommandResult) -> Either FSException (Maybe FSCommandResult)
runOnInitial action
  = evalState (runExceptT (unSt action)) initialState

-- | Test that creates file and show it's content.
createAndShowFile :: Either FSException (Maybe FSCommandResult)
createAndShowFile =
  runOnInitial (processFSCommand (CreateFile "kek") >>
                processFSCommand (ShowFile "kek"))

-- | Test that creates file twice.
createAlreadyExistFile :: Either FSException (Maybe FSCommandResult)
createAlreadyExistFile = runOnInitial $ do
  processFSCommand $ CreateFile "kek"
  processFSCommand $ CreateFile "kek"

-- | Test that creates and then removes file.
deleteExistingFile :: Either FSException (Maybe FSCommandResult)
deleteExistingFile = runOnInitial $ do
  processFSCommand $ CreateFile "kek"
  processFSCommand $ RemoveFile "kek"

-- | Test that creates file, than removes it and than reads it.
deleteAndShowFile :: Either FSException (Maybe FSCommandResult)
deleteAndShowFile = runOnInitial $ do
  processFSCommand $ CreateFile "kek"
  processFSCommand $ RemoveFile "kek"
  processFSCommand $ ShowFile "kek"

-- | Test that creates dir and show it's content.
createAndShowDir :: Either FSException (Maybe FSCommandResult)
createAndShowDir =
  runOnInitial (processFSCommand (CreateDirectory "dir") >>
                processFSCommand (ShowDirectory $ Just "dir"))

-- | Test that creates dir twice.
createAlreadyExistDir :: Either FSException (Maybe FSCommandResult)
createAlreadyExistDir = runOnInitial $ do
  processFSCommand $ CreateDirectory "dir"
  processFSCommand $ CreateDirectory "dir"

-- | Test that creates and deletes dir.
deleteExistingDir :: Either FSException (Maybe FSCommandResult)
deleteExistingDir = runOnInitial $ do
  processFSCommand $ CreateDirectory "dir"
  processFSCommand $ RemoveDirectory "dir"

-- | Test that creates dir, than deletes it and after that shows it.
deleteAndShowDir :: Either FSException (Maybe FSCommandResult)
deleteAndShowDir = runOnInitial $ do
  processFSCommand $ CreateDirectory "dir"
  processFSCommand $ RemoveDirectory "dir"
  processFSCommand $ ShowDirectory $ Just "dir"

-- | Creates file and writes str to it.
writeAndShow :: Either FSException (Maybe FSCommandResult)
writeAndShow = runOnInitial $ do
  processFSCommand $ CreateFile "file"
  processFSCommand $ WriteFile "file" "string"
  processFSCommand $ ShowFile "file"

-- | Run tests on file creation.
spec_FileCreationTest :: Spec
spec_FileCreationTest = do
  it "unexisting file creation doesn't fail" $ do
    runOnInitial (processFSCommand $ CreateFile "kek") `shouldBe` Right Nothing

  it "after file creation file exist and is empty" $ do
    createAndShowFile `shouldBe` Right (Just (FileContent' ""))

  it "creation of already existing file should fail with exception" $ do
    createAlreadyExistFile `shouldBe` Left (CannotCreateFile "kek" "file already exists")

-- | Run tests on file deletion.
spec_FileDeletionTest :: Spec
spec_FileDeletionTest = do
  it "deleting of unexisting file should fail with exception" $ do
    runOnInitial (processFSCommand $ RemoveFile "kek")
      `shouldBe` Left (CannotRemoveFile "kek" "file doesn't exist")

  it "deleting existing file doesn't fail" $ do
    deleteExistingFile `shouldBe` Right Nothing

  it "trying to show deleted file leads to exception" $ do
    deleteAndShowFile `shouldBe` Left (FileDoesntExist "kek" "file doesn't exist")

-- | Run tests on dir creation.
spec_DirectoryCreationTest :: Spec
spec_DirectoryCreationTest = do
  it "unexisting directory creation doesn't fail" $ do
    runOnInitial (processFSCommand  $ CreateDirectory "dir") `shouldBe` Right Nothing

  it "existing directory creation leads to exception" $ do
    createAndShowDir `shouldBe` Right (Just (FileList' []))

  it "creation of already existing dir leads to exception" $ do
    createAlreadyExistDir `shouldBe` Left (CannotCreateDirectory "dir" "directory already exists")

-- | Run tests on dir deletion.
spec_DirectoryDeletionTest :: Spec
spec_DirectoryDeletionTest = do
  it "deleting of unexisting dir should fail with exception" $ do
    runOnInitial (processFSCommand $ RemoveDirectory "kek")
      `shouldBe` Left (CannotRemoveDirectory  "kek" "directory doesn't exist")

  it "deleting existing dir doesn't fail" $ do
    deleteExistingDir `shouldBe` Right Nothing

  it "trying to show deleted dir leads to exception" $ do
    deleteAndShowDir `shouldBe` Left (DirectoryDoesntExists "dir" "directory doesn't exist")

-- | Runs tests on writing to file.
spec_WritingToFile :: Spec
spec_WritingToFile = do
  it "writing to unexisting file leads to exception" $ do
    runOnInitial (processFSCommand $ WriteFile "file" "some cheering text")
      `shouldBe` Left (FileDoesntExist "file" "file doesn't exist")
  it "write file str, show file === str" $ do
    writeAndShow `shouldBe` Right (Just (FileContent' "string"))
