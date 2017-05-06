{-# LANGUAGE RecordWildCards  #-}

module Args (
  ArgsAWS(..)
, AWSCmd(..)
, ArgsQueryLogs(..)
, CLICmd(..)
, OutputFormat(..)
, parseCLICmd
) where


import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Parse
import           Options.Applicative as OA
import qualified Text.PrettyPrint.ANSI.Leijen as PP


-- | Top-level command
data CLICmd
  = CLICmdVersion
  | CLICmdAWS !ArgsAWS !AWSCmd -- ^ Make calls to AWS
  deriving (Eq, Show)

data AWSCmd
  = CmdListLogGroups -- ^ List all available log groups
  | CmdListStreams !String -- ^ List log streams of a log group
  | CmdQueryLogs !ArgsQueryLogs -- ^ Main use case, query logs
  deriving (Eq, Show)


-- | Arguments shared by commands that use AWS APIs
data ArgsAWS
  = ArgsAWS { argVerbose :: !Bool
            , argAWSRegion :: !String
            , argAWSProfile :: !(Maybe String)
            } deriving (Eq, Show)


data OutputFormat
  = JSONOutput
  | TextOutput
  deriving (Eq, Show)


-- | Arguments to query CW Logs
data ArgsQueryLogs
  = ArgsQueryLogs { argLogGroupName:: !String
                  , argLogStreamNames:: ![String]
                  , argOutputFormat:: !OutputFormat
                  , argStartTime:: !UTCTime
                  , argEndTime:: !(Maybe UTCTime)
                  , argFollow:: !Bool
                  , argFollowDelaySeconds:: !Int
                  , argIncludeEventMetadata:: !Bool
                  , argFilterPattern:: !(Maybe String)
                  } deriving (Eq, Show)


-- | default values that require IO actions to get
data CliParserDefaults
  = CliParserDefaults { defaultStartTime :: !UTCTime -- ^ default log query start time
                      , parseTimeArgument :: ReadM UTCTime
                      }


parseCmdVersion :: Parser CLICmd
parseCmdVersion = (\ () -> CLICmdVersion)
  <$> flag' ()
      ( long "version"
        <> short 'V'
        <> help "Print version and exit." )


parseCLICmdAWS :: CliParserDefaults
               -> Parser CLICmd
parseCLICmdAWS cpd = CLICmdAWS
  <$> parseArgsAWS
  <*> parseAWSCmd cpd


parseAWSCmd :: CliParserDefaults
            -> Parser AWSCmd
parseAWSCmd cpd =
  parseCmdListStreams <|> parseCmdListLogGroups <|> parseCmdQueryLogs cpd


parseCmdListLogGroups :: Parser AWSCmd
parseCmdListLogGroups = (\ () -> CmdListLogGroups)
  <$> flag' ()
      ( long "list-log-groups"
        <> short 'l'
        <> help "List all available log groups." )


parseCmdListStreams :: Parser AWSCmd
parseCmdListStreams = CmdListStreams
    <$> strOption
        ( long "list-streams"
       <> short 'L'
       <> metavar "log-group-name"
       <> help "List all available streams in a log group." )


parseCmdQueryLogs :: CliParserDefaults
                  -> Parser AWSCmd
parseCmdQueryLogs cpd = CmdQueryLogs <$> parseArgsQueryLogs cpd


parseOutputFormat :: ReadM OutputFormat
parseOutputFormat = eitherReader $ \s ->
  case s
    of "json" -> Right JSONOutput
       "text" -> Right TextOutput
       x      -> Left $ "Failed to parse output format from " <> x <> ", expected 'json' or 'text'"



parseArgsAWS :: Parser ArgsAWS
parseArgsAWS = ArgsAWS
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Run in a verbose mode, for debugging" )
  <*> strOption
      ( long "region"
     <> short 'r'
     <> value "us-east-1"
     <> showDefault
     <> help "AWS Region" )
  <*> optional (strOption
      ( long "profile"
     <> short 'p'
     <> help "AWS Profile" ))


parseArgsQueryLogs :: CliParserDefaults
                   -> Parser ArgsQueryLogs
parseArgsQueryLogs CliParserDefaults{..} = ArgsQueryLogs
  <$> strOption
      ( long "log-group-name"
     <> short 'g'
     <> help "CWL log group name" )
  <*>  many (strOption
      ( long "log-stream-name"
     <> short 's'
     <> help "CWL log stream name, can be given multiple times" ))
  <*> option parseOutputFormat
      ( long "output-format"
     <> short 'F'
     <> value TextOutput
     <> showDefaultWith (\TextOutput -> "text")
     <> help "Output format [text|json]" )
  <*> option parseTimeArgument
      ( long "start-time"
     <> short 'S'
     <> value defaultStartTime
     <> showDefaultWith formatIso8601DateTime
     <> help "Start time" )
  <*> optional (option parseTimeArgument
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
     <> value 3
     <> showDefault
     <> help "How many seconds to delay output by in the 'follow' mode" )
  <*> switch
      ( long "include-event-metadata"
     <> short 'm'
     <> help "Include event metadata in the output" )
  <*> optional (OA.argument str
      ( metavar "CWL filter pattern"
     <> help "E.g. '\"some-pattern\"'" ))


parseCLICmd :: IO CLICmd
parseCLICmd = do
  currentTime <- getCurrentTime
  currentTz <- getCurrentTimeZone
  let defaultStartTime = addUTCTime (- 60) currentTime -- look 1 min back

      parseTs = eitherReader $ \s -> case parseUTCTimeTzT currentTz currentTime s
                                       of Nothing -> Left $ "can't parse time " <> s
                                          Just t -> Right t

      cpd = CliParserDefaults defaultStartTime parseTs
      opts = info (helper <*> (parseCmdVersion <|> parseCLICmdAWS cpd))
             ( fullDesc
            <> header "Tail-like utility for Cloud Watch Logs."
            <> footerDoc (Just progDescDoc'))

  execParser opts

  where progDescDoc' =
          PP.text "Filter pattern syntax documentation: http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/FilterAndPatternSyntax.html"
          PP.<+> PP.linebreak
          PP.<$> PP.text "Search for a UUID:"
          PP.<$> PP.indent 2
                   (PP.text "aws-logs -g my-log-group '\"da240fb2-9b9d-4b80-bac2-f3452e937919\"'")
          PP.<+> PP.linebreak
          PP.<$> PP.text "Search JSON logs for ERRORs:"
          PP.<$> PP.indent 2
                   (PP.text "aws-logs -g my-log-group '{$.level = \"ERROR\"}'")
          PP.<+> PP.linebreak
          PP.<$> PP.text "List available log groups:"
          PP.<$> PP.indent 2
                   (PP.text "aws-logs -l" )
          PP.<+> PP.linebreak
          PP.<$> PP.text "List available streams in a log group:"
          PP.<$> PP.indent 2
                   (PP.text "aws-logs -L my-log-group")
          PP.<+> PP.linebreak
          PP.<$> PP.text "For JSON filtering/formatting please pipe output to 'jq' (https://stedolan.github.io/jq/manual/)"
          PP.<+> PP.linebreak
          PP.<$> PP.text ("Start/end time parameters support these formats: " <> show allSupportedDateTimeFormats)
          PP.<+> PP.linebreak
          PP.<$> PP.indent 2
                   (PP.text "E.g. [-S -15m] -- start from 15 min ago ")
