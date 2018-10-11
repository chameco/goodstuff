module Main where

import Good.Prelude

import Options.Applicative

import Good.Interfaces.Web

import qualified Good.Services.Host as Host
import qualified Good.Services.Booru as Booru
import qualified Good.Services.TL7 as TL7
import qualified Good.Services.Saturnal as Saturnal

newtype UnimplementedError = UnimplementedError Text
                           deriving (Show, Eq)
deriving anyclass instance Exception UnimplementedError

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

web :: Options -> IO ()
web options = do
  putStrLn startup
  serving (port options) $ do
    handling (Get "/") . pure $ Plaintext startup
    Host.api
    Booru.api
    TL7.api
    Saturnal.api
  where startup = splash <> "\nThis is a web interface node.\n"

db :: Options -> IO ()
db _ = do
  putStrLn $ splash <> "\nThis is a database node.\n"
  pure ()

mux :: Options -> IO ()
mux _ = do
  putStrLn $ splash <> "\nThis is a multiplexer node.\n"
  pure ()

crawl :: Options -> IO ()
crawl _ = do
  putStrLn $ splash <> "\nThis is a crawler node.\n"
  pure ()

repl :: Options -> IO ()
repl _ = do
  putStrLn $ splash <> "\nThis is a console interface node.\n"
  throwM $ UnimplementedError "Command is unimplemented"
  pure ()

data Options = Options { port :: Int
                       , cmd :: Options -> IO ()
                       } 

main :: IO ()
main = do
  opts <- execParser . flip info idm . (<**>helper) $ Options
    <$> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 3000 <> help "Port for HTTP interface")
    <*> subparser (mconcat [ command "web" (info (pure web) idm)
                           , command "db" (info (pure db) idm)
                           , command "mux" (info (pure crawl) idm)
                           , command "crawl" (info (pure crawl) idm)
                           , command "repl" (info (pure repl) idm)
                           ])
  cmd opts opts
