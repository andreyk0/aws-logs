{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where


import qualified Control.Concurrent as CC
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Network.AWS.Auth (credFile)
import           Network.AWS.CloudWatchLogs
import           Network.AWS.Data
import           Numeric.Natural
import           Options.Applicative as OA
import           System.Environment (lookupEnv)
import           System.IO


data App =
  App { appRegion:: String
      , appAwsProfile:: Maybe String
      , appLogGroupName:: String
      , appLogStreamNames:: [String]
      , appOutputFormat:: String
      , appStartTime:: String
      , appEndTime:: Maybe String
      , appFollow:: Bool
      , appFollowDelaySeconds:: Int
      , appVerbose:: Bool
      , appIncludeEventMetadata:: Bool
      , appFilterPattern:: Maybe String
      } deriving (Eq, Show)


instance A.ToJSON FilteredLogEvent where
  toJSON e = A.object
    (catMaybes
      [ (("IngestionTime" A..=) . formatTimestamp) <$> ( e ^. fleIngestionTime )
      , ("LogStreamName"  A..=)                    <$> ( e ^. fleLogStreamName )
      , ("Message"        A..=)                    <$> ( messageJSONValue e    )
      , (("Timestamp"     A..=) . formatTimestamp) <$> ( e ^. fleTimestamp     )
      , ("EventId"        A..=)                    <$> ( e ^. fleEventId       )
      ])


parseApp :: UTCTime
         -> Parser App
parseApp defaultStartTime = App
     <$> strOption
         ( long "region"
        <> short 'r'
        <> value "us-east-1"
        <> showDefault
        <> help "AWS Region" )
     <*> (optional $ strOption
         ( long "profile"
        <> short 'p'
        <> help "AWS Profile" ))
     <*> strOption
         ( long "log-group-name"
        <> short 'g'
        <> help "CWL log group name" )
     <*> (many $strOption
         ( long "log-stream-name"
        <> short 's'
        <> help "CWL log stream name, can be given multiple times" ))
     <*> strOption
         ( long "output-format"
        <> short 'F'
        <> value "text"
        <> showDefault
        <> help "Output format [text|json]" )
     <*> strOption
         ( long "start-time"
        <> short 'S'
        <> value (formatUTCTime defaultStartTime)
        <> showDefault
        <> help "Start time" )
     <*> (optional $ strOption
         ( long "end-time"
        <> short 'E'
        <> help "End time" ))
     <*> switch
         ( long "follow"
        <> short 'f'
        <> help "Keep looking for more results. N.B. this can skip over some events" )
     <*> option auto
         ( long "follow-delay-seconds"
        <> short 'd'
        <> value 10
        <> showDefault
        <> help "How many seconds to delay output by in the 'follow' mode" )
     <*> switch
         ( long "verbose"
        <> short 'v'
        <> help "Run in a verbose mode, for debugging" )
     <*> switch
         ( long "include-event-metadata"
        <> short 'm'
        <> help "Include event metadata in the output" )
     <*> (optional $ OA.argument str
         ( metavar "CWL filter pattern"
        <> help "E.g. '\"some-pattern\"' http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/FilterAndPatternSyntax.html" ))


main :: IO ()
main = do
  currentTime <- getCurrentTime
  let defaultStartTime = addUTCTime (- (fromInteger 60)) currentTime -- look 1 min back
      p = parseApp defaultStartTime
  execParser (opts p) >>= tailAWSLogs
  where
    opts p = OA.info (helper <*> p)
      ( fullDesc
     <> header "Tail-like utility for Cloud Watch Logs."
     <> footer "E.g. aws-logs -g my-log-group '\"da240fb2-9b9d-4b80-bac2-f3452e937919\"' OR aws-logs -g my-log-group '{$.level = \"ERROR\"}'" )


tailAWSLogs:: App -> IO ()
tailAWSLogs App{..} = do
  when (appFollow && (isJust appEndTime)) $ error $ "-f (follow) conflicts with -E (end time)"
  when (appFollowDelaySeconds < 5) $ error "Log follow delay must be >= 5 seconds"

  r <- case fromText (T.pack appRegion)
         of Left e -> error e
            Right r' -> return r'

  let logLevel = if appVerbose
                 then Debug
                 else Info

  printResults <- case appOutputFormat
                    of "json" -> if appIncludeEventMetadata
                                 then return printLogEventJSON
                                 else return printLogMessageJSON
                       "text" -> if appIncludeEventMetadata
                                 then return printLogEventText
                                 else return printLogMessageText
                       x      -> error $ "Unsupported output format " <> x

  lgr <- newLogger logLevel stdout

  let profSectionFromFile s = do credF <- credFile
                                 return $ FromFile (T.pack s) credF

  awsEnvDefaultProfile <- lookupEnv "AWS_DEFAULT_PROFILE" -- older convention
  awsEnvProfile <- lookupEnv "AWS_PROFILE" -- newer convention
  credsFrom <- (head . catMaybes) [ profSectionFromFile <$> appAwsProfile
                                  , profSectionFromFile <$> awsEnvProfile
                                  , profSectionFromFile <$> awsEnvDefaultProfile
                                  , Just (return Discover)
                                  ]

  env <- (newEnv r credsFrom) <&> (envLogger .~ lgr)

  startTime <- case parseUTCTime appStartTime
                 of Left e -> error $ "Unable to parse start time " <> e
                    Right t -> return t

  endTime <- forM appEndTime $ \et ->
    case parseUTCTime et
      of Left e -> error $ "Unable to parse end time: " <> e
         Right t -> return t

  let logStreamNames = if (DL.null appLogStreamNames)
                       then Nothing
                       else Just $ NEL.fromList $ T.pack <$> appLogStreamNames

      fle = filterLogEvents (T.pack appLogGroupName)
              & (fleFilterPattern .~ (T.pack <$> appFilterPattern))
              . (fleLogStreamNames .~ logStreamNames)
              . (fleStartTime .~ Just (utcToNat startTime))
              . (fleEndTime .~ (utcToNat <$> endTime))


  runResourceT . runAWST env $ do
    -- paginate results, return latest event timestamp
    let  processResults fle' = paginate fle'
                                 =$= CL.concatMap (view flersEvents)
                                 $$ CL.mapM_ (liftIO . printResults)

         -- stop a few sec before current time, to give CW Logs agent time to send logs to service
         nextEndTs = do currentTime <- liftIO $ getCurrentTime
                        return $ (utcToNat currentTime) - (fromInteger . toInteger) (appFollowDelaySeconds * 1000)

         followResultsSince ts = do liftIO $ hPutStrLn stderr $ "Sleeping for " <> (show appFollowDelaySeconds) <> " seconds ..."
                                    liftIO $ CC.threadDelay $ appFollowDelaySeconds * 1000000
                                    endTs <- nextEndTs
                                    liftIO $ hPutStrLn stderr $ "Searching for more events " <> formatTimestamp ts <> " .. " <> formatTimestamp endTs
                                    _ <- processResults $ fle & (fleStartTime .~ Just ts)
                                                              . (fleEndTime .~ Just endTs)

                                    followResultsSince (endTs+1) -- keep searching

    if appFollow
    then followResultsSince (utcToNat startTime)
    else processResults fle


-- | Text output
printLogEventText:: FilteredLogEvent -> IO ()
printLogEventText e = do
  let msgHeader = catMaybes [ formatTimestamp <$> (e ^. fleTimestamp)
                            , T.unpack <$> (e ^. fleLogStreamName)
                            ]
      message = e ^. fleMessage

  putStr $ "[" <> (concat $ DL.intersperse " " msgHeader) <> "] "
  forM_ message (TIO.putStrLn)


-- | Message only, no event metadata
printLogMessageText:: FilteredLogEvent -> IO ()
printLogMessageText e = forM_ (e ^. fleMessage) TIO.putStrLn


-- | One JSON object per line
printLogEventJSON:: FilteredLogEvent -> IO ()
printLogEventJSON e = L8.putStrLn $ A.encode e


-- | One JSON object per line, message only, no metadata
printLogMessageJSON:: FilteredLogEvent -> IO ()
printLogMessageJSON e = forM_ (messageJSONValue e) $ L8.putStrLn . A.encode


-- | If Message is already JSON-formatted, unescape it
messageJSONValue:: FilteredLogEvent -> Maybe A.Value
messageJSONValue e =
  (do m <- e ^. fleMessage
      (A.decode . L.fromStrict . TE.encodeUtf8) m
  ) <|> (A.String <$> e ^. fleMessage) -- kep it in the original form if not JSON


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
