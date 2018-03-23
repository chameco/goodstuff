{-# LANGUAGE QuasiQuotes #-}

module Good.Services.KOL where

import Good.Prelude

import Text.Regex.PCRE.Heavy

import Good.Utilities.Curl
import Good.Utilities.Scraping

newtype KOLError = KOLError Text deriving Show
instance Exception KOLError

login :: (MonadIO m, MonadThrow m) => Text -> Text -> Curl m ()
login user pass = void $ curlPostRaw "https://www.kingdomofloathing.com/login.php" [("loginname", toSL user), ("password", toSL pass), ("loggingin", "Yup."), ("secure", "0")]

logout :: (MonadIO m, MonadThrow m) => Curl m ()
logout = void $ curlGetRaw "https://www.kingdomofloathing.com/logout.php"

pwdhash :: (MonadIO m, MonadThrow m) => Curl m Text
pwdhash = do charpane <- curlGetRaw "https://www.kingdomofloathing.com/charpane.php"
             case headMay $ scan [re|var pwdhash = "([0-9a-f]+)";|] charpane of
                 Just (_, [pwdhash]) -> pure $ toSL pwdhash
                 _ -> throwM $ KOLError "Could not extract pwdhash from charpane.php"

sendchat :: (MonadIO m, MonadThrow m) => Text -> Text -> Text -> Curl m ()
sendchat pid pwdhash msg = void . curlGetHTML . toSL $ mconcat ["https://www.kingdomofloathing.com/submitnewchat.php?playerid=", pid, "&pwd=", pwdhash, "&graf=", msg, "&j=1"]
