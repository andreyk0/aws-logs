{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}


module Main where

import           App
import           Args
import qualified Control.Concurrent as CC
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.AWS
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.AWS.CloudWatchLogs
import           TimeUtil


main :: IO ()
main = runApp $ do
  ArgsAWS{..} <- getAWSArgs
  cmd <- getAWSCmd

  case cmd
    of  CmdListLogGroups -> listLogGroups
        CmdListStreams g -> listLogStreams g
        CmdQueryLogs a   -> queryLogs a


listLogGroups :: App ()
listLogGroups = do
  let dlg = describeLogGroups
  paginate dlg =$= CL.concatMap (view dlgrsLogGroups)
               =$= CL.map (view lgLogGroupName)
               =$= CL.filter (isJust)
               =$= CL.map (fromJust)
               $$ CL.mapM_ (liftIO . TIO.putStrLn)


listLogStreams :: String
               -> App ()
listLogStreams g = do
  let dls = describeLogStreams (T.pack g)
  paginate dls =$= CL.concatMap (view dlsrsLogStreams)
               =$= CL.map (view lsLogStreamName)
               =$= CL.filter (isJust)
               =$= CL.map (fromJust)
               $$ CL.mapM_ (liftIO . TIO.putStrLn)


queryLogs :: ArgsQueryLogs
          -> App ()
queryLogs ArgsQueryLogs{..} = do

  when (argFollow && (isJust argEndTime)) $ error $ "-f (follow) conflicts with -E (end time)"
  when (argFollowDelaySeconds < 1) $ error "Log follow delay must be >= 1 seconds"


  let logStreamNames = if (DL.null argLogStreamNames)
                       then Nothing
                       else Just $ NEL.fromList $ T.pack <$> argLogStreamNames


      fle = filterLogEvents (T.pack argLogGroupName)
              & (fleFilterPattern .~ (T.pack <$> argFilterPattern))
              . (fleLogStreamNames .~ logStreamNames)
              . (fleStartTime .~ Just (utcToNat argStartTime))
              . (fleEndTime .~ (utcToNat <$> argEndTime))


      processResults fle' =
        paginate fle' =$= CL.concatMap (view flersEvents) $$ CL.mapM_ printOutput


      followResultsSince ts = do
        $(logInfo) $ "Sleeping for " <> (tshow argFollowDelaySeconds) <> " seconds ..."
        liftIO $ CC.threadDelay $ argFollowDelaySeconds * 1000000

        endTs <- nextQueryEndTimestamp argFollowDelaySeconds

        $(logInfo) $ "Searching for more events " <> (T.pack . formatTimestamp) ts <>
                     " .. " <> (T.pack . formatTimestamp) endTs

        _ <- processResults $ fle & (fleStartTime .~ Just ts) .
                                    (fleEndTime .~ Just endTs)

        followResultsSince (endTs+1) -- keep searching

  if argFollow
  then followResultsSince (utcToNat argStartTime)
  else processResults fle
