module Main where

import Good.Prelude

import Options.Applicative

import Good.Interfaces.Web

import qualified Good.Services.Host as Host
import qualified Good.Services.Booru as Booru
import qualified Good.Services.TL7 as TL7
import qualified Good.Services.Saturnal as Saturnal
import qualified Good.Services.Coal.Snapshot as Coal.Snapshot

splash :: Text
splash = "\
\   _____                 _     _          __  __ \n\
\  / ____|               | |   | |        / _|/ _|\n\
\ | |  __  ___   ___   __| |___| |_ _   _| |_| |_ \n\
\ | | |_ |/ _ \\ / _ \\ / _` / __| __| | | |  _|  _|\n\
\ | |__| | (_) | (_) | (_| \\__ \\ |_| |_| | | | |  \n\
\  \\_____|\\___/ \\___/ \\__,_|___/\\__|\\__,_|_| |_|  \n\
\\n\
\Goodstuff is a fully automated distributed data retrieval and archival system.\n\
\The system scrapes interesting artifacts from a variety of sources (HTTP, IRC,\n\
\Gopher, assorted P2P networks) and stores them for later perusal and/or analysis.\n\
\\n\
\Additionally, Goodstuff provides a number of auxiliary services related to data\n\
\transfer and retrieval. These include temporary public-facing file hosting and a\n\
\pastebin.\n\
\"

newtype WebOptions = WebOptions
  { webPort :: Int
  } 

webOptions :: Parser WebOptions
webOptions = WebOptions
  <$> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 3000 <> help "Port for HTTP interface")

web :: WebOptions -> IO ()
web o = do
  putStrLn startup
  serving (webPort o) $ do
    handling (Get "/") . pure $ Plaintext startup
    Host.api
    Booru.api
    TL7.api
    Saturnal.api
  where startup = splash <> "\nThis is a general-purpose web interface node.\n"

data CoalSnapshotOptions = CoalSnapshotOptions
  { coalSnapshotPort :: Int
  , coalSnapshotUser :: Text
  , coalSnapshotPass :: Text
  } 

coalSnapshotOptions :: Parser CoalSnapshotOptions
coalSnapshotOptions = CoalSnapshotOptions
  <$> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 3000 <> help "Port for HTTP interface")
  <*> strOption (long "user" <> short 'u' <> metavar "USER" <> help "KoL bot username")
  <*> strOption (long "pass" <> short 'P' <> metavar "PASS" <> help "KoL bot password")

coalSnapshot :: CoalSnapshotOptions -> IO ()
coalSnapshot o = do
  putStrLn startup
  serving (coalSnapshotPort o) $ do
    handling (Get "/") . pure $ Redirect "/coal/snapshot"
    Coal.Snapshot.api (coalSnapshotUser o) (coalSnapshotPass o)
    TL7.api
  where startup = splash <> "\nThis is a web interface node hosting the KoL snapshot server.\n"

data Options = Web WebOptions
             | CoalSnapshot CoalSnapshotOptions

options :: Parser Options
options = subparser $ mconcat
  [ command "web" (info (Web <$> webOptions) (progDesc "Launch general-purpose web server"))
  , command "coal-snapshot" (info (CoalSnapshot <$> coalSnapshotOptions) (progDesc "Launch KoL snapshot server"))
  ]

run :: Options -> IO ()
run (Web o) = web o
run (CoalSnapshot o) = coalSnapshot o

main :: IO ()
main = execParser opts >>= run
  where opts = info (options <**> helper) $ mconcat
          [ fullDesc
          , header "goodstuff - automation framework"
          ]
