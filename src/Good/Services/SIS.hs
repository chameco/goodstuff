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

import System.IO (hPutStrLn, hPrint)

import Network.Wai.Middleware.Cors (simpleCors)

import Good.Utilities.Web
import Good.Utilities.Scraping
import Good.Utilities.Scraping.Curl

newtype SISError = SISError Text deriving Show
instance Exception SISError

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
        crns <- param "crns[]" >>= fromJSON
        param "crns[]" >>= liftIO . hPutStrLn stderr . toSL
        status <- scraping $ register rin pass crns
        pure (JSON status)
    handling (Get "/sis/foo") $ pure (Plaintext "bar")

login :: (MonadIO m, MonadThrow m) => Text -> Text -> Scraping Curl m Text
login rin pass = do void $ postRaw "https://sis.rpi.edu/rss/twbkwbis.P_ValLogin" [("sid", toSL rin), ("PIN", toSL pass)]
                    welcome <- postRaw "https://sis.rpi.edu/rss/twbkwbis.P_ValLogin" [("sid", toSL rin), ("PIN", toSL pass)]
                    case headMay $ scan [re|Welcome,\+(.*),\+to|] welcome of
                        Just (_, [name]) -> pure . replace "+" " " $ toSL name
                        _ -> throwM $ SISError "Failed to login"

register :: (MonadIO m, MonadThrow m) => Text -> Text -> [Text] -> Scraping Curl m [Text]
register _ _ crns = liftIO (hPrint stderr crns) >> pure crns

mainmenu :: (MonadIO m, MonadThrow m) => Scraping Curl m HTML
mainmenu = getHTML "https://sis.rpi.edu/rss/twbkwbis.P_GenMenu?name=bmenu.P_StuMainMnu"
