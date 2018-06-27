{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module OutputPrinter (
  OutputPrinter
, printLogEventJSON
, printLogEventText
, printLogMessageJSON
, printLogMessageText
) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           Network.AWS.CloudWatchLogs
import           Options.Applicative as OA
import           TimeUtil


type OutputPrinter = forall m . (MonadIO m) => FilteredLogEvent -> m ()


instance A.ToJSON FilteredLogEvent where
  toJSON e = A.object
    (catMaybes
      [ ("IngestionTime" A..=) . formatTimestamp <$> ( e ^. fleIngestionTime )
      , ("LogStreamName" A..=)                   <$> ( e ^. fleLogStreamName )
      , ("Message"       A..=)                   <$> messageJSONValue e
      , ("Timestamp"     A..=) . formatTimestamp <$> ( e ^. fleTimestamp     )
      , ("EventId"       A..=)                   <$> ( e ^. fleEventId       )
      ])


-- | Text output
printLogEventText :: OutputPrinter
printLogEventText e = liftIO $ do
  let msgHeader = catMaybes [ formatTimestamp <$> (e ^. fleTimestamp)
                            , T.unpack <$> (e ^. fleLogStreamName)
                            ]
      message = e ^. fleMessage

  putStr $ "[" <> unwords msgHeader <> "] "
  forM_ message TIO.putStrLn


-- | Message only, no event metadata
printLogMessageText :: OutputPrinter
printLogMessageText e = liftIO $
  forM_ (e ^. fleMessage) TIO.putStrLn


-- | One JSON object per line
printLogEventJSON :: OutputPrinter
printLogEventJSON e = liftIO $
  L8.putStrLn $ A.encode e


-- | One JSON object per line, message only, no metadata
printLogMessageJSON :: OutputPrinter
printLogMessageJSON e = liftIO $
  forM_ (messageJSONValue e) $ L8.putStrLn . A.encode


-- | If Message is already JSON-formatted, unescape it
messageJSONValue :: FilteredLogEvent
                 -> Maybe A.Value
messageJSONValue e =
  (do m <- e ^. fleMessage
      (A.decode . L.fromStrict . TE.encodeUtf8) m
  ) <|> (A.String <$> e ^. fleMessage) -- kep it in the original form if not JSON
