{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TimeUtil (
  formatTimestamp
, formatUTCTime
, iso8601FmtStr
, natToUTC
, nextQueryEndTimestamp
, parseUTCTime
, utcToNat
) where


import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Numeric.Natural


utcToNat:: UTCTime -> Natural
utcToNat t = (fromInteger . round) $ (toRational (utcTimeToPOSIXSeconds t)) * 1000

natToUTC:: Natural -> UTCTime
natToUTC n = posixSecondsToUTCTime $ ((fromInteger . toInteger) n) / 1000

formatTimestamp:: Natural -> String
formatTimestamp n = formatUTCTime  (natToUTC n)

formatUTCTime:: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale iso8601FmtStr

parseUTCTime:: String -> Either String UTCTime
parseUTCTime s =
  case parseTimeM False defaultTimeLocale iso8601FmtStr s :: Maybe UTCTime
    of Nothing -> Left $ "failed to parse " <> s
       Just t -> Right t

iso8601FmtStr:: String
iso8601FmtStr = "%Y-%m-%dT%H:%M:%S%Z"


-- | CW logs query end time.
-- For incremental runs. We don't want to query
-- all the way up to 'now' as it may not have received
-- all data yet
nextQueryEndTimestamp :: (MonadIO m)
                      => Int -- ^ seconds before now
                      -> m Natural
nextQueryEndTimestamp secBeforeNow = do
  currentTime <- liftIO $ getCurrentTime
  return $ (utcToNat currentTime) - (fromInteger . toInteger) (secBeforeNow * 1000)
