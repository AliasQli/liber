{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Display where

import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Encoding
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Haskell.TH
import Parser.Eval
import Parser.Parse
import Parser.ToTemplate
import Parser.Type hiding (vars)
import Path
import System.Exit
import System.IO
import Terminal
import Text.Read (readMaybe)

ms :: Int
ms = 1000

sR :: [Char]
sR = "\x1b[31m"

eR :: [Char]
eR = "\x1b[0m"

data Fixed = Fixed
  { chapters :: Vector Chapter
  , mvar :: MVar Cmd
  }
  deriving (Eq)

data Vary = Vary
  { vars :: Status
  , chaIx :: Int
  , senIx :: Int
  }
  deriving (Read, Show, Eq)

data Cmd = Exit | Save | Load | Str String

nextSen :: MonadState Vary m => m ()
nextSen = modify (\vary@Vary{..} -> vary{senIx = senIx + 1})

(??) :: Eq a => Vector (a, b) -> a -> Maybe Int
vector ?? a = f (length vector - 1) a vector
  where
    f n a vector
      | n < 0 = Nothing
      | (a', _b') <- vector V.! n
        , a == a' =
        Just n
      | otherwise = f (n -1) a vector

getChapterIx :: Vector ([Char], b) -> [Char] -> Int
getChapterIx chapters title = fromMaybe (error $ "No Caput " <> title <> " found!") $ chapters ?? title

type Display = ReaderT Fixed (StateT Vary IO)

putStrSlowly :: String -> Display Bool
putStrSlowly "" = return False
putStrSlowly xxs@(x : xs) =
  if x == '\x1b'
    then do
      liftIO (putChar '\x1b')
      s <- liftIO (putUntilM xs)
      putStrSlowly' s
    else putStrSlowly' xxs
  where
    putStrSlowly' :: String -> Display Bool
    putStrSlowly' "" = return False
    putStrSlowly' (x : xs) = do
      liftIO (putChar x)
      let next = do
            liftIO $threadDelay (30 * ms)
            putStrSlowly xs
      Fixed{..} <- ask
      may <- liftIO (tryTakeMVar mvar)
      case may of
        Just Exit -> exit
        Just Save -> save >> next
        Just Load -> load
        Just _ -> liftIO (putStr xs) $> False
        Nothing -> next
    putUntilM [] = return []
    putUntilM (x : xs) =
      putChar x
        >> if x == 'm' then return xs else putUntilM xs

putStrsSlowly :: [String] -> Display Bool
putStrsSlowly [] = return False
putStrsSlowly (s : ss) = do
  reload <- putStrSlowly s
  if reload
    then return True
    else do
      Fixed{..} <- ask
      cmd <- liftIO (takeMVar mvar)
      case cmd of
        Exit -> exit
        Load -> load
        Save -> save >> putStrsSlowly ss
        _ -> putStrsSlowly ss

exit :: Display a
exit = liftIO $ do
  putChar '\n'
  exitSuccess

save :: Display ()
save = do
  vary <- get
  liftIO $ do
    eth <- try $ do
      path <- concatRelativePath "libersav"
      writeFile path $ show vary
    case eth of
      Left (e :: SomeException) -> putStr "*Can't save*"
      Right _ -> putStr "*Saved*"

load :: Display Bool
load = do
  eth <- liftIO $
    try $ do
      path <- concatRelativePath "libersav"
      read <$> readFile path
  case eth of
    Left (e :: SomeException) -> liftIO (putStr "*Can't load*") $> False
    Right vary -> put vary >> liftIO (putStr "*Loaded*") $> True

process :: Display ()
process = do
  Vary{..} <- get
  Fixed{..} <- ask
  let Just (_title, sentences) = chapters V.!? chaIx
      maybeSen = sentences V.!? senIx
  when (isNothing maybeSen) $ liftIO exitSuccess
  let sen = fromJust maybeSen
  let procSen = \case
        Speak maybeP words -> do
          forM_ maybeP (liftIO . putStrLn)
          reload <- putStrsSlowly words
          liftIO (putStr "\n\n")
          unless reload nextSen
        Electio var choices -> do
          forM_ (zip [1 ..] choices) $ \(i, s) -> liftIO $ putStrLn (show i <> s)
          liftIO enableEcho
          let loop = do
                cmd <- liftIO (takeMVar mvar)
                case cmd of
                  Exit -> exit
                  Save -> save >> loop
                  Load -> load
                  Str s -> do
                    if
                        | Just i <- readMaybe s
                          , i > 0
                          , i <= length choices -> do
                          let vars' = M.update (const $ Just i) var vars
                          modify (\vary@Vary{..} -> vary{vars = vars'})
                          return False
                        | otherwise -> loop
          reload <- loop
          liftIO disableEcho
          liftIO (putChar '\n')
          unless reload $ do
            nextSen
        Si b sen ->
          if eval vars b
            then procSen sen
            else nextSen
        Age commands -> do
          let (vars', may) = action vars commands
          modify (\vary@Vary{..} -> vary{vars = vars'})
          case may of
            Nothing -> nextSen
            Just title ->
              modify (\vary@Vary{..} -> vary{chaIx = getChapterIx chapters title, senIx = 0})
  procSen sen
  process

getScript utf8Script = either (error . show) id $ parseScript (decodeUtf8 utf8Script)

display (Script vars chapters) = do
  hSetBuffering stdout NoBuffering
  disableEcho
  let chaIx = getChapterIx chapters "Primum"
  mvar <- newEmptyMVar
  forkIO $
    forever $ do
      line <- getLine
      putMVar mvar $ case line of
        "q" -> Exit
        "s" -> Save
        "l" -> Load
        _ -> Str line
  putChar '\n'
  let fixed = Fixed chapters mvar
  let vary = Vary vars chaIx 0
  runStateT (runReaderT process fixed) vary

embedScript path = do
  file <- runIO $ B.readFile path
  let script = getScript file
  toTemplate script