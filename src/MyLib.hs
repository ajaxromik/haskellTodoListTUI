{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib where

import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center as C
import Graphics.Vty as V
import Lens.Micro (_1)
import Lens.Micro.Mtl (use, (.=))
import Lens.Micro.TH (makeLenses)

data Screen = Menu | Calendar | Dailies | Reminders deriving (Eq, Show)

data TuiState =
  TuiState {
    _currentScreen :: Screen
  }
  deriving (Show, Eq)

makeLenses ''TuiState

data WidgetName = MWidget | CWidget | DWidget | RWidget deriving (Ord, Eq, Show)

tuiApp :: App TuiState e WidgetName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = return ()
    , appAttrMap = const $ attrMap V.defAttr []
    }

buildInitialState :: IO TuiState
buildInitialState = pure TuiState { _currentScreen = Menu }

drawTui :: TuiState -> [Widget WidgetName]
drawTui ts = case _currentScreen ts of
    Menu -> [
        joinBorders $
        withBorderStyle unicode $
        B.border $
        C.center (C.center (str "Left") <+> B.vBorder <+> C.center (str "Right"))]
    _ -> [withBorderStyle unicode $ center (str $ show (_currentScreen ts))]

handleTuiEvent :: BrickEvent WidgetName e -> EventM WidgetName TuiState ()
handleTuiEvent e =
    case e of
        VtyEvent (V.EvKey (KChar 'q') []) -> halt
        VtyEvent (V.EvKey (KChar 'x') []) -> currentScreen .= Menu
        VtyEvent (V.EvKey (KChar 'c') []) -> currentScreen .= Calendar
        VtyEvent (V.EvKey (KChar 'd') []) -> currentScreen .= Dailies
        VtyEvent (V.EvKey (KChar 'r') []) -> currentScreen .= Reminders
        _ -> return ()

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState
