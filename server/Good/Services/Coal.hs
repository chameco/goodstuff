{-# LANGUAGE QuasiQuotes #-}

module Good.Services.Coal where

import Good.Prelude

import Text.Regex.PCRE.Heavy

import Good.Architecture.Scraper
import Good.Architecture.Scrapers.Native

newtype KOLError = KOLError Text deriving Show
instance Exception KOLError

login :: (MonadIO m, MonadCatch m) => Text -> Text -> Scraping Native m ()
login user pass = void $ postRaw "https://www.kingdomofloathing.com/login.php" [("loginname", toSL user), ("password", toSL pass), ("loggingin", "Yup."), ("secure", "0")]

logout :: (MonadIO m, MonadCatch m) => Scraping Native m ()
logout = void $ getRaw "https://www.kingdomofloathing.com/logout.php"

pwdhash :: (MonadIO m, MonadCatch m) => Scraping Native m Text
pwdhash = do charpane <- getRaw "https://www.kingdomofloathing.com/charpane.php"
             case headMay $ scan [re|var pwdhash = "([0-9a-f]+)";|] charpane of
               Just (_, [ph]) -> pure $ toSL ph
               _ -> throwM $ KOLError "Could not extract pwdhash from charpane.php"
