{-# LANGUAGE RecordWildCards #-}

module Good.Services.Coal.Chat where

import Good.Prelude

import Control.Concurrent (threadDelay)

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))

import Good.Architecture.Scraper
import Good.Architecture.Scrapers.Native
import Good.Services.Coal

data ChatSender = ChatSender { color :: Maybe Text
                             , id :: Text
                             , name :: Text
                             } deriving (Show, Eq, Generic)
instance FromJSON ChatSender where

data ChatMessage = ChatMessage { channel :: Maybe Text
                               , channelcolor :: Maybe Text
                               , format :: Maybe Text
                               , mid :: Maybe Text
                               , msg :: Text
                               , time :: Text
                               , publicity :: Text
                               , who :: Maybe ChatSender
                               } deriving (Show, Eq)
instance FromJSON ChatMessage where
  parseJSON = withObject "post" $ \o -> do
    channel <- o .:? "channel"
    channelcolor <- o .:? "channelcolor"
    format <- o .:? "format"
    mid <- o .:? "mid"
    msg <- o .: "msg"
    time <- o .: "time"
    publicity <- o .: "type"
    who <- o .:? "who"
    pure ChatMessage {..}

data ChatResponse = ChatResponse { delay :: Integer
                                 , last :: Text
                                 , msgs :: [ChatMessage]
                                 } deriving (Show, Eq, Generic)
instance FromJSON ChatResponse where

sendchat :: (MonadIO m, MonadCatch m) => Text -> Text -> Text -> Scraping Native m ()
sendchat pid ph msg = void . getHTML . toSL $ mconcat ["https://www.kingdomofloathing.com/submitnewchat.php?playerid=", pid, "&pwd=", ph, "&graf=", msg, "&j=1"]

recvchat :: (MonadIO m, MonadCatch m) => Text -> Scraping Native m ChatResponse
recvchat time = getJSON . toSL $ mconcat ["https://www.kingdomofloathing.com/newchatmessages.php?j=1&lasttime=", show time]

chat :: (MonadIO m, MonadCatch m) => Text -> Text -> (ChatMessage -> IO ()) -> Scraping Native m ()
chat user pass handler = login user pass >> go "0"
  where go :: (MonadIO m, MonadCatch m) => Text -> Scraping Native m ()
        go time = do res <- recvchat time
                     liftIO (mapM_ handler (msgs res) >> threadDelay 5000000)
                     go (Good.Services.Coal.Chat.last res)

formatReadable :: ChatMessage -> IO ()
formatReadable m | publicity m == "public" = putStrLn $ mconcat ["[", fromMaybe "" (channel m), "] ", maybe "" name (who m), ": ", msg m]
                 | publicity m == "private" = putStrLn $ mconcat ["PM from ", maybe "" name (who m), ": ", msg m]
                 | otherwise = putStrLn (msg m)
