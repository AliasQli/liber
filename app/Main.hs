{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.FileEmbed
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Encoding
import Parse
import System.IO
import Terminal
import Text.Read (readMaybe)

ms = 1000

sR = "\x1b[31m"

eR = "\x1b[0m"

putStrSlowly :: MVar a -> String -> IO ()
putStrSlowly _ "" = return ()
putStrSlowly mvar xxs@(x : xs) =
  if x == '\x1b'
    then
      putChar '\x1b'
        >> putUntilM xs
        >>= putStrSlowly' mvar
    else putStrSlowly' mvar xxs
  where
    putStrSlowly' _ "" = return ()
    putStrSlowly' mvar (x : xs) =
      putChar x
        >> tryTakeMVar mvar
        >>= \case
          Just _ -> putStr xs
          Nothing ->
            threadDelay (30 * ms)
              >> putStrSlowly mvar xs
    putUntilM [] = return []
    putUntilM (x : xs) =
      putChar x
        >> if x == 'm' then return xs else putUntilM xs

process :: [Chapter] -> MVar String -> Status -> [Sentence] -> IO ()
process _ _ _ [] = return ()
process chapters mvar vars (Speak may words : sentences) = do
  forM_ may putStrLn
  forM_ words $ \s -> putStrSlowly mvar s >> takeMVar mvar
  putStr "\n\n"
  process chapters mvar vars sentences
process chapters mvar vars (Electio var choices : sentences) = do
  forM_ (zip [1 ..] choices) \(i, s) -> putStrLn (show i <> s)
  enableEcho
  let loop = do
        may <- readMaybe <$> takeMVar mvar
        if
            | Just i <- may
              , i > 0
              , i <= length choices ->
              return $ M.update (const $ Just i) var vars
            | otherwise -> loop
  vars' <- loop
  disableEcho
  putChar '\n'
  process chapters mvar vars' sentences
process chapters mvar vars (Si b sen : sentences) = do
  let sentences' = if eval vars b then sen : sentences else sentences
  process chapters mvar vars sentences'
process chapters mvar vars (Age commands : sentences) = do
  let (vars', may) = action vars commands
  case may of
    Nothing -> process chapters mvar vars' sentences
    Just title ->
      process chapters mvar vars' $
        fromMaybe (error $ "No Caput " <> title <> " found!") $ lookup title chapters

scriptFile = decodeUtf8 $(embedFile "script.scr")

main = do
  hSetBuffering stdout NoBuffering
  disableEcho
  let Script vars chapters = either (error . show) id $ parseScript scriptFile
  let sentences = fromMaybe (error "No Caput Primum found!") $ lookup "Primum" chapters
  mvar <- newEmptyMVar
  forkIO $
    forever $ do
      line <- getLine
      putMVar mvar line
  putChar '\n'
  process chapters mvar vars sentences
