module Good.Services.Coal.Adventure where

import Good.Prelude

import Good.Architecture.Scraper
import Good.Architecture.Scrapers.Native

adventure :: (MonadIO m, MonadCatch m) => Text -> Scraping Native m HTML
adventure loc = getHTML . toSL $ mconcat ["https://www.kingdomofloathing.com/adventure.php?snarfblat=", loc]
