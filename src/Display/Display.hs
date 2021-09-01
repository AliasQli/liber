{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Display.Display where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor         (($>))
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           Display.Save
import           Display.Terminal
import           Display.Type
import           MegaParser.Parse
import           Orphans              ()
import           System.IO

data WaitFor
  = None
  | WaitNext Bool
  | End

display :: Vector Record -> IO ()
display records = void $ do
  stdinBuffering <- hGetBuffering stdin
  stdoutBuffering <- hGetBuffering stdout
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  disableEcho
  mvar <- newEmptyMVar
  forkIO $
    forever $ do
      c <- getChar
      putMVar mvar $ case c of
        's'  -> Save
        'l'  -> Load
        'a'  -> Next
        ' '  -> Next
        '\n' -> Next
        _    -> Other c
  runDisplay mvar $ process records
  enableEcho
  hSetBuffering stdin stdinBuffering
  hSetBuffering stdout stdoutBuffering

process :: Vector Record -> Display ()
process records = go >> newLine >> waitForNext False >> newLine
  where
    len = V.length records
    go = dispChr >>= \case
      None     -> checkMVar >>= \case
        None       -> wait >> go
        WaitNext b -> waitForNext b >> go
        End        -> return ()
      WaitNext b -> waitForNext b >> go
      End      -> return ()
    checkMVar = ask >>= liftIO . tryTakeMVar >>= \case
      Just Save -> newLine >> save $> WaitNext False
      Just Load -> newLine >> load $> WaitNext False
      Just Next -> dispSen
      _         -> return None
    waitForNext b = ask >>= liftIO . takeMVar >>= \case
      Save -> when b newLine >> save >> waitForNext b
      Load -> when b newLine >> load >> waitForNext b
      Next -> return ()
      _    -> waitForNext b
    dispChr :: Display WaitFor
    dispChr = do
      Pos{..} <- get
      let Record maybeSpeaker sens = records V.! _recIx
          sen = sens V.! _senIx
          chr = sen `T.index` _chrIx
      when ((_senIx == 0 || _justSL) && _chrIx == 0 && (isNothing maybeSpeaker || maybeSpeaker /= _lastSpeaker)) $ do
        newLine
        mapM_ putTextLn maybeSpeaker
      justSL .= False
      liftIO $ putChar chr
      if
          | _chrIx < T.length sen - 1  -> (chrIx += 1) $> None
          | _senIx < V.length sens - 1 -> chrIx .= 0 >> (senIx += 1) $> WaitNext True
          | _recIx < len           - 1 -> chrIx .= 0 >> senIx .= 0 >> recIx += 1 >>
                                          lastSpeaker .= maybeSpeaker >> newLine $> WaitNext False
          | otherwise                  -> put initialPos $> End
    dispSen :: Display WaitFor
    dispSen = do
      Pos{..} <- get
      let Record maybeSpeaker sens = records V.! _recIx
          sen = sens V.! _senIx
      -- when (_senIx == 0 && _chrIx == 0) $ do
      --   newLine
      --   mapM_ putTextLn maybeSpeaker
      -- Won't!
      chrIx .= 0
      putText $ snd $ T.splitAt _chrIx sen
      if
          | _senIx < V.length sens - 1 -> (senIx += 1) $> WaitNext True
          | _recIx < len - 1           -> senIx .= 0 >> (recIx += 1) >> lastSpeaker .= maybeSpeaker >>
                                          newLine $> WaitNext False
          | otherwise                  -> put initialPos $> End
