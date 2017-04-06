{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module App (
  App
, getAWSArgs
, getAWSCmd
, isVerbose
, printOutput
, runApp
, tshow
) where


import           Args
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.GitRev
import           Network.AWS.Auth (credFile)
import           Network.AWS.CloudWatchLogs (FilteredLogEvent)
import           Network.AWS.Data
import           OutputPrinter
import           System.Environment (lookupEnv)
import           System.Exit
import           System.IO


data AppState =
  AppState { asAWSEnv :: !Env
           , asAWSArgs :: !ArgsAWS
           , asAWSCmd :: !AWSCmd
           , asOutputPrinter :: !(FilteredLogEvent -> App ())
           }


newtype App a =
  App { unApp :: ReaderT AppState (AWST' AppState (ResourceT (LoggingT IO))) a
      } deriving ( Applicative
                 , Functor
                 , Monad
                 , MonadBase IO
                 , MonadCatch
                 , MonadIO
                 , MonadLogger
                 , MonadLoggerIO
                 , MonadMask
                 , MonadReader AppState
                 , MonadResource
                 , MonadThrow
                 )

-- AWST' doesn't integrate with MonadLogger by default, add a couple of instances,
-- default implementations should be fine
instance MonadLogger m => MonadLogger (AWST' r m)
instance MonadLoggerIO m => MonadLoggerIO (AWST' r m)


-- | Amazonka AWS environment
instance HasEnv AppState where
  environment f as@AppState{..} = fmap (\e -> as { asAWSEnv = e}) (f asAWSEnv)


isVerbose :: App Bool
isVerbose = fmap argVerbose getAWSArgs

getAWSArgs :: App ArgsAWS
getAWSArgs = App $ fmap asAWSArgs Control.Monad.Trans.Reader.ask

getAWSCmd :: App AWSCmd
getAWSCmd = App $ fmap asAWSCmd Control.Monad.Trans.Reader.ask

printOutput :: FilteredLogEvent
            -> App ()
printOutput fle = do
  p <- App $ fmap asOutputPrinter Control.Monad.Trans.Reader.ask
  p fle


tshow :: (Show a)
      => a
      -> Text
tshow = T.pack . show


runApp :: App a
       -> IO a
runApp appA  = do
  cmd <- parseCLICmd

  case cmd
    of CLICmdVersion ->
         die $ "Version: " <> $(gitBranch) <> "@" <> $(gitHash)

       CLICmdAWS awsa awsc ->
         runAWSCmd appA awsa awsc


runAWSCmd :: App a
          -> ArgsAWS
          -> AWSCmd
          -> IO a
runAWSCmd appA awsa@ArgsAWS{..} awsc = do

  awsEnvProfile <- lookupEnv "AWS_PROFILE"

  let profSectionFromFile s = do credF <- credFile
                                 return $ FromFile (T.pack s) credF

  credsFrom <- (head . catMaybes) [ profSectionFromFile <$> argAWSProfile
                                  , profSectionFromFile <$> awsEnvProfile
                                  , Just (return Discover)
                                  ]

  awsLogger <- newLogger (if argVerbose then Debug else Info) stdout -- for debugging the tool only

  awsRegion <- case fromText (T.pack argAWSRegion)
                 of Left e -> error e
                    Right r' -> return r'

  env <- (newEnv credsFrom) <&>
           (if argVerbose then (envLogger .~ awsLogger) else id) .
           (envRegion .~ awsRegion)

  let maybeQLogArgs = case awsc
                        of CmdQueryLogs a -> Just a
                           _              -> Nothing

      includeMeta = maybe False argIncludeEventMetadata maybeQLogArgs

      -- printer is only really needed when querying logs, just default to text elsewhere
      resultsPrinter = case maybe TextOutput argOutputFormat maybeQLogArgs
                         of JSONOutput -> if includeMeta
                                          then printLogEventJSON
                                          else printLogMessageJSON
                            TextOutput -> if includeMeta
                                          then printLogEventText
                                          else printLogMessageText

      appState = (AppState env awsa awsc resultsPrinter)

      logLevelFilter _ ll = if argVerbose then True else (ll >= LevelInfo)

  runStderrLoggingT $ filterLogger logLevelFilter $
    runResourceT $ runAWST appState $ runReaderT (unApp appA) appState
