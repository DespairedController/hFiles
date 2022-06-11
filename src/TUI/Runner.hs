-- | Runs TUI. And makes all the dirty work of rendering.
-- Supports much less than @CLI.Runner@, but looks quite nice.
module TUI.Runner
  ( run,
  )
where

import Brick
  ( App (..),
    BrickEvent (VtyEvent),
    EventM,
    Next,
    ViewportScroll (vScrollBy),
    ViewportType (Vertical),
    Widget,
    attrMap,
    attrName,
    continue,
    defaultMain,
    fg,
    hBox,
    hLimit,
    halt,
    showFirstCursor,
    str,
    strWrap,
    vBox,
    vLimit,
    viewport,
    viewportScroll,
    withAttr,
  )
import qualified Brick.Main as M
import Brick.Types ()
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimitPercent, vLimitPercent)
import CLI.Types (CliProcess (unStIoRef))
import Control.Exception (try)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    ReaderT (runReaderT),
  )
import Core.CommandProcessing (processFSCommand)
import Core.Types
  ( DirectoryInfo,
    FSCommand (..),
    FSCommandResult
      ( DirectoryInfo',
        FileContent',
        FileInfo',
        FileList'
      ),
    FSException (..),
    FileInfo,
    toFSPath,
  )
import Cursor.Simple.List.NonEmpty
  ( makeNonEmptyCursor,
    nonEmptyCursorCurrent,
    nonEmptyCursorNext,
    nonEmptyCursorPrev,
    nonEmptyCursorSelectNext,
    nonEmptyCursorSelectPrev,
  )
import Data.IORef (newIORef)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe
import Data.Text (pack)
import qualified Data.Text as T
import Graphics.Vty
  ( Event (EvKey),
    Key (KChar, KDown, KEnter, KLeft, KRight, KUp),
    defAttr,
    red,
  )
import qualified Graphics.Vty as V
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.Exit (die)
import qualified System.Time.Extra
import TUI.Types (Name (..), TuiShellState (..))
import qualified Data.List

-- | Runs TUI.
run :: IO ()
run = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  putStrLn "END"

-- | Brick app.
tuiApp :: App TuiShellState e Name
tuiApp =
  App
    {
      -- Declares what to render.
      appDraw = drawTui,
      -- Cursor. I don't have any cursor interations.
      appChooseCursor = showFirstCursor,
      -- Declares how to handle events (key pressing).
      appHandleEvent = handleTuiEvent,
      -- Declares initial state.
      appStartEvent = pure,
      -- Declares some attributes for rendering.
      -- The only thing we have here is red color
      -- for currently selected entry in entry list.
      appAttrMap = const $ attrMap defAttr [(attrName "selected", fg red)]
    }

-- | Initial state of the app:
-- ioref points to current directory file path,
-- selected entry is the top of the current directorie's entry list,
-- not showing anything in the @Viewer@.
buildInitialState :: IO TuiShellState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- getDirectoryContents here
  ioref <- newIORef $ toFSPath here
  case NE.nonEmpty $ Data.List.sort contents of
    Nothing -> die "never happens"
    Just ne ->
      pure TuiShellState {realState = ioref, tuiStatePaths = makeNonEmptyCursor ne, showingFile = Nothing}

-- | Just a declaring of how to draw entries.
-- Selected entry will be rendered red.
drawPath :: Bool -> FilePath -> Widget n
drawPath b =
  ( if b
      then withAttr $ attrName "selected"
      else id
  )
    . str

-- | @Viewer@ scroller.
viewerScroll :: ViewportScroll Name
viewerScroll = viewportScroll Viewer

-- | @CurrentDirectory@ scroller. 
currentDirectoryScroll :: ViewportScroll Name
currentDirectoryScroll = viewportScroll CurrentDirectory

-- | Handles all the events.
-- If something wents wrong accidentlly, shows error in @Viewer@.
handleTuiEvent :: TuiShellState -> BrickEvent Name e -> EventM Name (Next TuiShellState)
handleTuiEvent s e =
  case e of
    VtyEvent vty ->
      case vty of
        -- Quit on 'q' pressing.
        EvKey (KChar 'q') [] -> halt s

        -- On KDown scrolls @CurrentDirectory@ down on one entry
        -- (if there is where to scroll). Moves cursor's focus
        -- on next entry (if there is one).
        EvKey KDown [] -> do
          let nec = tuiStatePaths s
          vScrollBy currentDirectoryScroll 1
            >> case nonEmptyCursorSelectNext nec of
              Nothing -> continue s
              Just nec' -> continue $ s {tuiStatePaths = nec'}

        -- On KUp scrolls @CurrentDirectory@ up on one entry
        -- (if there is where to scroll). Moves cursor's focus
        -- on previous entry (if there is one).
        EvKey KUp [] -> do
          let nec = tuiStatePaths s
          vScrollBy currentDirectoryScroll (-1)
            >> case nonEmptyCursorSelectPrev nec of
              Nothing -> continue s
              Just nec' -> continue $ s {tuiStatePaths = nec'}

        -- Press 'i' to show info about selected file/folder in @Viewer@.
        EvKey (KChar 'i') [] -> do
          let current = nonEmptyCursorCurrent $ tuiStatePaths s
          a <-
            liftIO
              ( try
                  ((runReaderT $ unStIoRef $ processFSCommand (Info current)) (realState s)) ::
                  IO (Either FSException (Maybe FSCommandResult))
              )
          case a of
            Right res ->
              case res of
                Just (FileInfo' info) -> continue $ s {showingFile = Just $ show info}
                Just (DirectoryInfo' info) -> continue $ s {showingFile = Just $ show info}
            Left e -> continue $ s {showingFile = Just $ show e}

        -- Press Enter to go to selected folder or show selected file in @Viewer@.
        EvKey KEnter [] -> do
          let current = nonEmptyCursorCurrent $ tuiStatePaths s
          a <-
            liftIO
              ( try
                  ((runReaderT $ unStIoRef $ processFSCommand (Show current)) (realState s)) ::
                  IO (Either FSException (Maybe FSCommandResult))
              )
          case a of
            Right res -> case res of
              Nothing -> continue $ s {showingFile = Nothing}
              Just (FileContent' content) -> continue $ s {showingFile = Just content}
              Just (FileList' list) ->
                case NE.nonEmpty list of
                  Nothing -> liftIO $ die "never happens"
                  Just ne -> continue $ s {tuiStatePaths = makeNonEmptyCursor ne}
            Left e -> continue $ s {showingFile = Just $ show e}

        -- Scrolls @Viewer@ down on one line.
        EvKey KRight [] -> vScrollBy viewerScroll 1 >> continue s

        -- Scrolls @Viewer@ up on one line.
        EvKey KLeft [] -> vScrollBy viewerScroll (-1) >> continue s

        -- Deletes selected entry and updates @CurrentDirectory@ entry list.
        EvKey (KChar 'd') [V.MCtrl] -> do
          let current = nonEmptyCursorCurrent $ tuiStatePaths s
          a <-
            liftIO
              ( try ((runReaderT $ unStIoRef (processFSCommand (Remove current))) (realState s)) ::
                  IO (Either FSException (Maybe FSCommandResult))
              )
          case a of
            Right (Just (FileList' list)) ->
              case NE.nonEmpty list of
                Nothing -> liftIO $ die "never happens"
                Just ne -> continue $ s {tuiStatePaths = makeNonEmptyCursor ne}
            Right _ -> liftIO $ die "nope"
            Left e -> continue $ s {showingFile = Just $ show e}
        _ -> continue s
    _ -> continue s

-- | Draws state.
drawTui :: TuiShellState -> [Widget Name]
drawTui ts =
  [ C.center $
      B.border $
        hLimitPercent 100 $
          vLimitPercent 100 $
            vBox [pair, B.hBorder, helper]
  ]
  where
    helper =
      hBox
        [ hLimit 24 $ borderWithLabel (str "q") (str "quit"),
          hLimit 24 $ borderWithLabel (str "Enter") (str "view/cd"),
          hLimit 24 $ borderWithLabel (str "i") (str "show info"),
          hLimit 24 $ borderWithLabel (str "<- ->") (str "scroll viewer"),
          hLimit 24 $ borderWithLabel (str "Up/Down") (str "scroll dirs"),
          hLimit 24 $ borderWithLabel (str "C-d") (str "delete")
        ]
    nec = tuiStatePaths ts
    file = Data.Maybe.fromMaybe " " (showingFile ts)
    pair =
      hBox
        [ viewport CurrentDirectory Vertical $
            vBox $
              concat
                [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec,
                  [drawPath True $ nonEmptyCursorCurrent nec],
                  map (drawPath False) $ nonEmptyCursorNext nec
                ],
          B.vBorder,
          viewport Viewer Vertical $ strWrap file
        ]
