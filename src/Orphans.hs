module Orphans () where

import           Data.Binary           (Binary (get, put))
import           Data.Time.Clock       (UTCTime, nominalDiffTimeToSeconds,
                                        secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)
import           Data.Vector           (Vector)
import qualified Data.Vector           as V

instance Binary a => Binary (Vector a) where
  get = V.fromList <$> get
  put = put . V.toList

minutesSinceEpoch :: UTCTime -> Int
minutesSinceEpoch = floor . (/ 60) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

minutesToUTC :: Int -> UTCTime
minutesToUTC = posixSecondsToUTCTime . secondsToNominalDiffTime . (*60.0) . fromIntegral

instance Binary UTCTime where
  get = minutesToUTC <$> get
  put = put . minutesSinceEpoch
