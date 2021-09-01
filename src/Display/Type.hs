{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Display.Type where

import           Control.Concurrent
import           Control.Lens         (makeLenses)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Binary          (Binary)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           Orphans              ()

ms :: Int
ms = 1000

data Pos
  = Pos
      { _recIx  :: Int
      , _senIx  :: Int
      , _chrIx  :: Int
      , _justSL :: Bool
      , _lastSpeaker :: Maybe Text
      }
    deriving (Generic, Binary)

makeLenses ''Pos

initialPos :: Pos
initialPos = Pos 0 0 0 False Nothing 

data Command
  = Save
  | Load
  | Next
  | Other Char
  deriving (Eq)

type Display = StateT Pos (ReaderT (MVar Command) IO)

runDisplay :: MVar Command -> Display a -> IO (a, Pos)
runDisplay mvar m = runReaderT (runStateT m initialPos) mvar

newLine :: MonadIO m => m ()
newLine = liftIO $ putChar '\n'

wait :: MonadIO m => m ()
wait = liftIO $ threadDelay (30 * ms)

putText :: MonadIO m => Text -> m ()
putText = liftIO . putStr . T.unpack

putTextLn :: MonadIO m => Text -> m ()
putTextLn = liftIO . putStrLn . T.unpack
