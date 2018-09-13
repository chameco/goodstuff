module Good.Architecture.Scraper where

import Good.Prelude

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)

import Text.HTML.TagSoup

import Good.Architecture.Error

type HTML = [Tag Text]

class Scraper b where
    type family Scraping (b :: *) = (t :: (* -> *) -> * -> *) | t -> b
    scraping :: (MonadIO m, MonadCatch m) => Scraping b m a -> m a
    getRaw :: (MonadIO m, MonadCatch m) => Text -> Scraping b m ByteString
    getJSON :: (MonadIO m, MonadCatch m, FromJSON a) => Text -> Scraping b m a
    getHTML :: (MonadIO m, MonadCatch m, Functor (Scraping b m)) => Text -> Scraping b m HTML
    getHTML = fmap toHTML . getRaw
    postRaw :: (MonadIO m, MonadCatch m) => Text -> [(ByteString, ByteString)] -> Scraping b m ByteString
    postJSON :: (MonadIO m, MonadCatch m, FromJSON a) => Text -> [(ByteString, ByteString)] -> Scraping b m a
    postHTML :: (MonadIO m, MonadCatch m, Functor (Scraping b m)) => Text -> [(ByteString, ByteString)] -> Scraping b m HTML
    postHTML path params = toHTML <$> postRaw path params

fromJSON :: (MonadCatch m, FromJSON a) => Text -> m a
fromJSON = throwLeft (DecodeError . toSL) . eitherDecode . toSL

toJSON :: ToJSON a => a -> Text
toJSON = toSL . encode

toHTML :: ByteString -> HTML
toHTML = parseTags . toSL

beneath :: Text -> HTML -> HTML
beneath t = dropWhile (~/= (toSL t :: String))

several :: Text -> HTML -> [HTML]
several t = sections (~== (toSL t :: String))
