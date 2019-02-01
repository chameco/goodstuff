module Good.Services.Coal.Fight where

import Good.Prelude

import Good.Architecture.Scraper
import Good.Architecture.Scrapers.Native

macro :: (MonadIO m, MonadCatch m) => Text -> Scraping Native m HTML
macro m = postHTML (mconcat ["https://www.kingdomofloathing.com/fight.php"]) [("action", "macro"), ("macrotext", toSL m)]
