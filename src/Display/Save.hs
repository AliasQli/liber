{-# LANGUAGE DeriveAnyClass #-}

module Display.Save where

import           Control.Concurrent   (takeMVar)
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Binary          (Binary, decodeFile, encodeFile)
import           Data.Char            (ord)
import           Data.Maybe           (fromJust, isJust)
import           Data.Time.Clock      (UTCTime, getCurrentTime)
import           Display.Type
import           GHC.Generics         (Generic)
import           Orphans              ()
import           Path                 (relativeToSelf)
import           System.Directory

data SaveItem
  = SaveItem
      { time   :: UTCTime
      , _recIx :: Int
      -- , _senIx :: Int
      }
    deriving (Generic, Binary)

showSaveItem :: SaveItem -> String
showSaveItem = show . time

newtype SaveFile = SaveFile (Maybe SaveItem, Maybe SaveItem, Maybe SaveItem)
    deriving (Generic)
    deriving anyclass (Binary)

initialSaveFile :: SaveFile
initialSaveFile = SaveFile (Nothing, Nothing, Nothing)

saveFileToList :: SaveFile -> [Maybe SaveItem]
saveFileToList (SaveFile (s1, s2, s3)) = [s1, s2, s3]

listToSaveFile :: [Maybe SaveItem] -> SaveFile
listToSaveFile l = SaveFile (s1, s2, s3)
  where
    [s1, s2, s3] = take 3 $ l ++ repeat Nothing

showsSaveFile :: SaveFile -> [String]
showsSaveFile = zipWith (:) ['1'..] . fmap f . saveFileToList
  where
    f = (". " ++) <$> maybe "-" showSaveItem

makeSaveItem :: Display SaveItem
makeSaveItem = do
  time <- liftIO getCurrentTime
  Pos{..} <- get
  return SaveItem{..}

loadSaveItem :: SaveItem -> Display ()
loadSaveItem SaveItem{..} = do
  let _chrIx = 0
      _senIx = 0
      _justSL = False
      _lastSpeaker = Nothing
  put Pos{..}

getSaveFile :: MonadIO m => m SaveFile
getSaveFile = liftIO $ do
  path <- relativeToSelf "save.sav"
  b <- doesFileExist path
  if b
    then decodeFile path
    else return initialSaveFile

putSaveFile :: MonadIO m => SaveFile -> m ()
putSaveFile saveFile = liftIO $ do
  path <- relativeToSelf "save.sav"
  encodeFile path saveFile

waitUntilSaveNum :: Bool -> [Maybe SaveItem] -> Display (Maybe Int)
waitUntilSaveNum allowNothing saveList = go
  where
    len = length saveList
    go = ask >>= liftIO . takeMVar >>= \case
      Other c -> let i = ord c - ord '1' in if
        | c == '0' -> return Nothing
        | i >= 0, i < len, allowNothing || isJust (saveList !! i) -> return (Just i)
        | otherwise  -> go
      _ -> go

replace :: Int -> a -> [a] -> [a]
replace i x xs = p1 ++ [x] ++ p2
  where
    (p1, _:p2) = splitAt i xs

putStrLnIndent :: MonadIO m => [Char] -> m ()
putStrLnIndent = liftIO . putStrLn . ("  " ++)

save :: Display ()
save = do
  newLine
  putStrLnIndent "存档。您是在刻舟求剑。"
  newLine
  saveFile <- getSaveFile
  mapM_ putStrLnIndent (showsSaveFile saveFile)
  newLine
  putStrLnIndent "按对应数字存档。按0悔过。"
  let saveList = saveFileToList saveFile
  waitUntilSaveNum True saveList >>= \case
    Nothing -> putStrLnIndent "您悔过了。" >> chrIx .= 0
    Just i  -> do
      si <- makeSaveItem
      putSaveFile $ listToSaveFile $ replace i (Just si) saveList
      putStrLnIndent "已存档。"
  justSL .= True
  -- newLine

load :: Display ()
load = do
  newLine
  putStrLnIndent "读档。您妄图回溯时间。"
  newLine
  saveFile <- getSaveFile
  mapM_ putStrLnIndent (showsSaveFile saveFile)
  newLine
  putStrLnIndent "按对应数字读档。按0悔过。"
  let saveList = saveFileToList saveFile
  waitUntilSaveNum False saveList >>= \case
    Nothing -> putStrLnIndent "您悔过了。" >> chrIx .= 0
    Just i  -> do
      loadSaveItem $ fromJust $ saveList !! i
      putStrLnIndent "已读档。"
  justSL .= True
  -- newLine
