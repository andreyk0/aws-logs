{-# LANGUAGE OverloadedStrings #-}

module TimeUtil (
  formatTimestamp
, formatUTCTime
, iso8601FmtStr
, natToUTC
, nextQueryEndTimestamp
, parseUTCTime
, utcToNat
) where


import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import Numeric.Natural


iso8601FmtStr :: String
iso8601FmtStr = iso8601DateFormat (Just "%H:%M:%S%Z")


utcToNat :: UTCTime
         -> Natural
utcToNat t = (fromInteger . round) $ toRational (utcTimeToPOSIXSeconds t) * 1000


natToUTC :: Natural
         -> UTCTime
natToUTC n = posixSecondsToUTCTime $ (fromInteger . toInteger) n / 1000


formatTimestamp :: Natural
                -> String
formatTimestamp n = formatUTCTime  (natToUTC n)


formatUTCTime :: UTCTime
              -> String
formatUTCTime = formatTime defaultTimeLocale iso8601FmtStr


parseUTCTime :: TimeZone
             -> String
             -> Either String UTCTime
parseUTCTime currentTz s =
  case parseLocalFmt <|> parseZonedFmt
    of Nothing -> Left $ "failed to parse " <> s
       Just t -> Right t

  where parseZonedFmt = do t <- parseAny zonedTimeFormats
                           return $ zonedTimeToUTC t

        parseLocalFmt = do t <- parseAny localTimeFormats
                           return $ zonedTimeToUTC $ t { zonedTimeZone = currentTz }

        parseAny fmts = listToMaybe $ catMaybes $ (\fmt -> parseTimeM False defaultTimeLocale fmt s) <$> fmts

        localTimeFormats = [ iso8601DateFormat (Just "%H:%M:%S")
                           , iso8601DateFormat (Just "%H:%M")
                           , iso8601DateFormat (Just "%H")
                           , iso8601DateFormat Nothing
                           ]

        zonedTimeFormats = (<> "%Z") <$> localTimeFormats


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
