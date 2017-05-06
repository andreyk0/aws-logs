{-# LANGUAGE OverloadedStrings #-}

module TimeUtil (
  formatTimestamp
, natToUTC
, nextQueryEndTimestamp
, parseUTCTime
, utcToNat
) where


import Control.Monad.IO.Class
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Parse
import Numeric.Natural


utcToNat :: UTCTime
         -> Natural
utcToNat t = (fromInteger . round) $ toRational (utcTimeToPOSIXSeconds t) * 1000


natToUTC :: Natural
         -> UTCTime
natToUTC n = posixSecondsToUTCTime $ (fromInteger . toInteger) n / 1000


formatTimestamp :: Natural
                -> String
formatTimestamp n = formatIso8601DateTime (natToUTC n)


-- | CW logs query end time.
-- For incremental runs. We don't want to query
-- all the way up to 'now' as it may not have received
-- all data yet
nextQueryEndTimestamp :: (MonadIO m)
                      => Int -- ^ seconds before now
                      -> m Natural
nextQueryEndTimestamp secBeforeNow = do
  currentTime <- liftIO getCurrentTime
  return $ utcToNat currentTime - (fromInteger . toInteger) (secBeforeNow * 1000)
