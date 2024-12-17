{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module MyLib where

import GHC.Generics (Generic)
import System.IO
import System.Exit (exitFailure)
import Data.ByteString (ByteString, hGetSome, empty, isPrefixOf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Data.Csv.Incremental
import Data.Csv (FromRecord, ToRecord)
import Data.Time
import Data.Time.Calendar (addDays)

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

data Task = Task
  { name :: !ByteString,
    datetime  :: !ByteString
  } deriving (Show, Eq, Generic)

instance FromRecord Task
instance ToRecord Task

data TuiState =
  TuiState {
    _currentScreen :: Screen,
    _dailies :: [Task],
    _weeklies :: [Task]
  }


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

buildInitialState :: [Task] -> [Task] -> IO TuiState
buildInitialState d w = pure TuiState { _currentScreen = Menu, _dailies = d, _weeklies = w }

dailyTasks :: [Task] -> IO [Task]
dailyTasks tasks = do
    now <- getZonedTime
    let day = C8.pack(formatTime defaultTimeLocale "%Y-%m-%d" now) -- format and convert to BS
    return $ filter (\task -> day `isPrefixOf` datetime task) tasks

-- order is start, end, and the time to be checked
checkRange :: ByteString -> ByteString -> ByteString -> Bool
checkRange startBS endBS checkBS = 
    case (timeFromBS startBS, timeFromBS endBS, timeFromBS checkBS) of
        (Just start, Just end, Just check) -> check >= start && check <= end
        _ -> False

timeFromBS :: ByteString -> Maybe UTCTime
timeFromBS date = parseTimeM True defaultTimeLocale "%Y-%m-%d" (C8.unpack date)

cutOffEnd :: ByteString -> ByteString
cutOffEnd str = C8.pack $ fst (break (== ' ') (C8.unpack str))

weeklyTasks :: [Task] -> IO [Task]
weeklyTasks tasks = do
    today <- utctDay <$> getCurrentTime
    let start = addDays (-3) today
    let end = addDays 3 today
    let startDay = C8.pack (formatTime defaultTimeLocale "%Y-%m-%d" start)
    let endDay = C8.pack (formatTime defaultTimeLocale "%Y-%m-%d" end)
    print startDay
    -- print (checkRange startDay endDay (parseTimeM True defaultTimeLocale "%Y-%m-%d" (cutOffEnd $ C8.pack("2024-12-25 19:13:10"))))

    return $ filter (\task -> checkRange startDay endDay (cutOffEnd $ datetime task)) tasks

byteStringLines :: [Task] -> Widget n
byteStringLines tList = borderWithLabel (str "Tasks") $
  vBox (map (str . C8.unpack . name) tList)

drawTui :: TuiState -> [Widget WidgetName]
drawTui ts = case _currentScreen ts of
    Menu -> [
        joinBorders $
        withBorderStyle unicode $
        B.border $
        C.center (C.center (str "Press C to see the Calendar") <+> B.vBorder <+> C.center (str "Press D to see the daily tasks") 
        <+> B.vBorder <+> C.center (str "Press x to come back here\nor\nPress q to quit"))]
    Dailies -> [
        withBorderStyle unicode $
        B.border $
        C.center (byteStringLines (_dailies ts))]
    Calendar -> [
        withBorderStyle unicode $
        B.border $
        C.center (byteStringLines (_weeklies ts))]
    _ -> [withBorderStyle unicode $ center (str "error")]

handleTuiEvent :: BrickEvent WidgetName e -> EventM WidgetName TuiState ()
handleTuiEvent e =
    case e of
        VtyEvent (V.EvKey (KChar 'q') []) -> halt
        VtyEvent (V.EvKey (KChar 'x') []) -> currentScreen .= Menu
        VtyEvent (V.EvKey (KChar 'c') []) -> currentScreen .= Calendar
        VtyEvent (V.EvKey (KChar 'd') []) -> currentScreen .= Dailies
        -- VtyEvent (V.EvKey (KChar 'r') []) -> currentScreen .= Reminders
        _ -> return ()

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
                    Left _  -> error $ "Found Left, expected Right") input

tui :: IO ()
tui = do
--   text <- encode [Task ("Walk" :: ByteString) ("2024-12-16 18:13:08" :: ByteString)]
--   print text
  tasks <- cleaning $ readFromFile
--   print tasks -- finally have list of tasks after hours of debugging why the guides dont work
--   print $ cleaning $ readFromFile

  dailies <- dailyTasks $ tasks
  print dailies

  weeklies <- weeklyTasks $ tasks
  print weeklies

  initialState <- buildInitialState dailies weeklies 

  
  
  endState <- defaultMain tuiApp initialState
  print "Goodbye!"
--   print endState
