module Good.Services.SIS where

import Good.Prelude

import Good.Utilities.HTTP
import Good.Utilities.Scraping

newtype SISError = SISError Text deriving Show
instance Exception SISError

login :: (MonadIO m, MonadThrow m) => Text -> Text -> HTTP m ()
login rin pass = void $ httpPostRaw "http://sis.rpi.edu/rss/twbkwbis.P_ValLogin" [("sid", toSL rin), ("PIN", toSL pass)]

mainmenu :: (MonadIO m, MonadThrow m) => HTTP m ByteString
mainmenu = httpGetRaw "http://sis.rpi.edu/rss/twbkwbis.P_GenMenu?name=bmenu.P_StuMainMnu"
