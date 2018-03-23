module Good.Utilities.Scraping where

import Good.Prelude

import Text.HTML.TagSoup

import Good.Architecture.Input
import Good.Architecture.Inputs.HTTPGet

type HTML = [Tag Text]

toHTML :: ByteString -> HTML
toHTML = parseTags . toSL

getHTML :: (Input i, MonadIO m) => i -> Inputting i m HTML
getHTML i = toHTML <$> getRaw i

beneath :: Text -> HTML -> HTML
beneath t = dropWhile (~/= (toSL t :: String))

several :: Text -> HTML -> [HTML]
several t = sections (~== (toSL t :: String))
