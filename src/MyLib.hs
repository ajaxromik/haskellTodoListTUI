{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module MyLib where

import GHC.Generics (Generic)
import System.IO
import System.Exit (exitFailure)
import Data.ByteString (ByteString, hGetSome, empty)
import qualified Data.ByteString.Lazy as BL
import Data.Csv.Incremental
import Data.Csv (FromRecord, ToRecord)

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
        VtyEvent (V.EvKey (KChar 'd') []) -> do 
            currentScreen .= Dailies

        VtyEvent (V.EvKey (KChar 'r') []) -> currentScreen .= Reminders
        _ -> return ()

data Task = Task
  { name :: !ByteString,
    datetime  :: !ByteString
  } deriving (Show, Eq, Generic)

instance FromRecord Task
instance ToRecord Task

-- tasks :: [Task]
-- tasks = [Task "John Doe" "2024-12-16 17:41:40", Task "Smith" "2024-12-16 18:00:40"]

-- writeToFile :: IO ()
-- writeToFile = do
--   BL.writeFile "savefile.csv" $ encode $
--     foldMap encodeRecord tasks

feed :: (ByteString -> Parser Task) -> Handle -> IO (Parser Task)
feed k csvFile = do
  hIsEOF csvFile >>= \case -- checks if reached EOF
    True  -> return $ k empty
    False -> k <$> hGetSome csvFile 4096

readFromFile :: IO [Either String Task]
readFromFile = do
  withFile "savefile.csv" ReadMode $ \ csvFile -> do
    let loop !_ (Fail _ errMsg) = do putStrLn errMsg; exitFailure
        loop acc (Many rs k)    = loop (acc <> rs) =<< feed k csvFile
        loop acc (Done rs)      = return (acc <> rs)
    result <- loop [] (decode NoHeader)
    return result

cleaning :: IO [Either String Task] -> IO [Task]
cleaning ioIn = do
    input <- ioIn
    return $ map (\x -> case x of
                    Right val -> val
                    Left _  -> error "Found Left, expected Right") input

tui :: IO ()
tui = do
--   text <- encode [Task ("Walk" :: ByteString) ("2024-12-16 18:13:08" :: ByteString)]
--   print text
  tasks <- cleaning $ readFromFile
  print tasks -- finally have list of tasks after hours of debugging why the guides dont work
--   print $ cleaning $ readFromFile


  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState
