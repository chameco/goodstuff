module Main where

import Good.Prelude

import System.Environment (getEnv)

import Good.Interfaces.Web
-- import qualified Good.Services.SIS as SIS
import qualified Good.Services.Host as Host
-- import qualified Good.Services.Folio as Folio
import qualified Good.Services.TL7 as TL7
import qualified Good.Services.Saturnal as Saturnal

main :: IO ()
main = do
  -- folio <- Folio.api
  (Just port) <- readMay <$> getEnv "PORT"
  serving port $ do
    Host.api
    handling (Get "/") . pure $ Plaintext "\
\   _____                 _     _          __  __ \n\
\  / ____|               | |   | |        / _|/ _|\n\
\ | |  __  ___   ___   __| |___| |_ _   _| |_| |_ \n\
\ | | |_ |/ _ \\ / _ \\ / _` / __| __| | | |  _|  _|\n\
\ | |__| | (_) | (_) | (_| \\__ \\ |_| |_| | | | |  \n\
\  \\_____|\\___/ \\___/ \\__,_|___/\\__|\\__,_|_| |_|  \n\
\\n\
\Goodstuff is a fully automated data retrieval and archival system.\n\
\The system scrapes interesting artifacts from a variety of sources (HTTP, IRC,\n\
\Gopher, assorted P2P networks) and stores them for later perusal and/or analysis.\n\
\\n\
\Additionally, Goodstuff provides a number of auxiliary services related to data\n\
\transfer and retrieval. These include temporary public-facing file hosting and a\n\
\pastebin.\n\
                                          \"
    -- folio
    -- SIS.api
    TL7.api
    Saturnal.api
