{-# LANGUAGE QuasiQuotes #-}

module Good.Services.SIS (
    SISError,
    api,
    login,
    mainmenu
) where

import Good.Prelude

import Data.Text (replace)

import Text.Regex.PCRE.Heavy

import Network.Wai.Middleware.Cors (simpleCors)

import Good.Architecture.Scraper
import Good.Architecture.Scrapers.Curl
import Good.Interfaces.Web

api :: Serving IO ()
api = do
  middleware simpleCors
  handling (Post "/sis/name") $ do
    rin <- param "rin"
    pass <- param "pass"
    name <- scraping $ login rin pass
    pure (Plaintext name)
  handling (Post "/sis/register") $ do
    rin <- param "rin"
    pass <- param "pass"
    term <- param "term"
    crns <- param "crns" >>= fromJSON
    status <- scraping $ register rin pass term crns
    pure (JSON status)
  handling (Get "/sis/foo") . throwM $ WebError forbidden403 "Not allowed :("

newtype SISError = SISError Text deriving Show
instance Exception SISError

login :: (MonadIO m, MonadCatch m) => Text -> Text -> Scraping Curl m Text
login rin pass = do void $ postRaw "https://sis.rpi.edu/rss/twbkwbis.P_ValLogin" [("sid", toSL rin), ("PIN", toSL pass)]
                    welcome <- postRaw "https://sis.rpi.edu/rss/twbkwbis.P_ValLogin" [("sid", toSL rin), ("PIN", toSL pass)]
                    case headMay $ scan [re|Welcome,\+(.*),\+to|] welcome of
                      Just (_, [name]) -> pure . replace "+" " " $ toSL name
                      _ -> throwM $ SISError "Failed to login"

registerOne :: (MonadIO m, MonadCatch m) => Text -> Text -> Scraping Curl m (Maybe Text)
registerOne term crn = do void $ postRaw "https://sis.rpi.edu/rss/bwskfreg.P_AltPin" [("term_in", toSL term)]
                          res <- postRaw "https://sis.rpi.edu/rss/bwckcoms.P_Regs" [
                            ("term_in", toSL term), ("RSTS_IN", "DUMMY"), ("assoc_term_in", "DUMMY"), ("CRN_IN", "DUMMY"), ("start_date_in", "DUMMY"), ("end_date_in", "DUMMY"), ("SUBJ", "DUMMY"),
                            ("CRSE", "DUMMY"), ("SEC", "DUMMY"), ("LEVL", "DUMMY"), ("CRED", "DUMMY"), ("GMOD", "DUMMY"), ("TITLE", "DUMMY"), ("MESG", "DUMMY"), ("REG_BTN", "DUMMY"),
                            ("RSTS_IN", "RW"), ("CRN_IN", toSL crn), ("assoc_term_in", ""), ("start_date_in", ""), ("end_date_in", ""),
                            ("RSTS_IN", "RW"), ("CRN_IN", ""), ("assoc_term_in", ""), ("start_date_in", ""), ("end_date_in", ""),
                            ("RSTS_IN", "RW"), ("CRN_IN", ""), ("assoc_term_in", ""), ("start_date_in", ""), ("end_date_in", ""),
                            ("RSTS_IN", "RW"), ("CRN_IN", ""), ("assoc_term_in", ""), ("start_date_in", ""), ("end_date_in", ""),
                            ("RSTS_IN", "RW"), ("CRN_IN", ""), ("assoc_term_in", ""), ("start_date_in", ""), ("end_date_in", ""),
                            ("regs_row", "0"), ("wait_row", "0"), ("add_row", "10"), ("REG_BTN", "Submit+Changes") ]
                          pure $ if res =~ [re|Add Errors|] then Nothing else Just crn

register :: (MonadIO m, MonadCatch m) => Text -> Text -> Text -> [Text] -> Scraping Curl m [Text]
register rin pass term crns = do void $ login rin pass
                                 success <- mapM (registerOne term) crns
                                 pure $ catMaybes success

mainmenu :: (MonadIO m, MonadCatch m) => Scraping Curl m HTML
mainmenu = getHTML "https://sis.rpi.edu/rss/twbkwbis.P_GenMenu?name=bmenu.P_StuMainMnu"
