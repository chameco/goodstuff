{-# LANGUAGE RecordWildCards #-}

module Good.Services.Yotsuba where

import Good.Prelude

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))

import Good.Architecture.Scraper
import Good.Architecture.Scrapers.Native

type Catalog = [Page]

data Page = Page { page :: Integer
                 , threads :: [Post]
                 } deriving (Show, Eq, Generic)
instance FromJSON Page

type Thread = [Post]
newtype WrappedThread = WrappedThread { posts :: Thread
                                      } deriving (Show, Eq, Generic)
instance FromJSON WrappedThread

data Post = Post { number :: Integer
                 , timestamp :: Integer
                 , name :: Maybe Text
                 , tripcode :: Maybe Text
                 , subject :: Maybe Text
                 , body :: Maybe HTML
                 } deriving (Show, Eq)
instance FromJSON Post where
  parseJSON = withObject "post" $ \o -> do
    number <- o .: "no"
    timestamp <- o .: "time"
    name <- o .:? "name"
    tripcode <- o .:? "trip"
    subject <- o .:? "sub"
    body <- fmap (toHTML . (toSL :: Text -> ByteString)) <$> (o .:? "com")
    pure Post {..}

catalog :: (MonadIO m, MonadCatch m) => Text -> Scraping Native m Catalog
catalog board = getJSON $ mconcat ["https://a.4cdn.org/", board, "/catalog.json"]

thread :: (MonadIO m, MonadCatch m) => Text -> Integer -> Scraping Native m Thread
thread board no = do
  wt <- getJSON $ mconcat ["https://a.4cdn.org/", board, "/thread/", tshow no, ".json"]
  pure $ posts wt
