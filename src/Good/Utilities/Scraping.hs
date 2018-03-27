module Good.Utilities.Scraping where

import Good.Prelude

import Data.Aeson (FromJSON)

import Text.HTML.TagSoup

type HTML = [Tag Text]

class Scraper b where
    type family Scraping (b :: *) = (t :: (* -> *) -> * -> *) | t -> b
    scraping :: (MonadIO m, MonadThrow m) => Scraping b m a -> m a
    getRaw :: (MonadIO m, MonadThrow m) => Text -> Scraping b m ByteString
    getJSON :: (MonadIO m, MonadThrow m, FromJSON a) => Text -> Scraping b m a
    getHTML :: (MonadIO m, MonadThrow m, Functor (Scraping b m)) => Text -> Scraping b m HTML
    getHTML = fmap toHTML . getRaw
    postRaw :: (MonadIO m, MonadThrow m) => Text -> [(ByteString, ByteString)] -> Scraping b m ByteString
    postJSON :: (MonadIO m, MonadThrow m, FromJSON a) => Text -> [(ByteString, ByteString)] -> Scraping b m a
    postHTML :: (MonadIO m, MonadThrow m, Functor (Scraping b m)) => Text -> [(ByteString, ByteString)] -> Scraping b m HTML
    postHTML path params = toHTML <$> postRaw path params

toHTML :: ByteString -> HTML
toHTML = parseTags . toSL

beneath :: Text -> HTML -> HTML
beneath t = dropWhile (~/= (toSL t :: String))

several :: Text -> HTML -> [HTML]
several t = sections (~== (toSL t :: String))
