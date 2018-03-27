{-# LANGUAGE QuasiQuotes #-}

module Good.Services.SIS (
    SISError,
    login,
    mainmenu
) where

import Good.Prelude

import Data.Text (replace)

import Text.Regex.PCRE.Heavy

import Good.Utilities.Scraping
import Good.Utilities.Scraping.Curl

newtype SISError = SISError Text deriving Show
instance Exception SISError

login :: (MonadIO m, MonadThrow m) => Text -> Text -> Scraping Curl m Text
login rin pass = do void $ postRaw "https://sis.rpi.edu/rss/twbkwbis.P_ValLogin" [("sid", toSL rin), ("PIN", toSL pass)]
                    welcome <- postRaw "https://sis.rpi.edu/rss/twbkwbis.P_ValLogin" [("sid", toSL rin), ("PIN", toSL pass)]
                    case headMay $ scan [re|Welcome,\+(.*),\+to|] welcome of
                        Just (_, [name]) -> pure . replace "+" " " $ toSL name
                        _ -> throwM $ SISError "Failed to login"

mainmenu :: (MonadIO m, MonadThrow m) => Scraping Curl m HTML
mainmenu = getHTML "https://sis.rpi.edu/rss/twbkwbis.P_GenMenu?name=bmenu.P_StuMainMnu"
