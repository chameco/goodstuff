module Main where

import Good.Prelude

import System.Environment (getEnv)

import Good.Interfaces.Web
import qualified Good.Services.SIS as SIS
import qualified Good.Services.Folio as Folio

main :: IO ()
main = do
  folio <- Folio.api
  (Just port) <- readMay <$> getEnv "PORT"
  serving port $ do
    handling (Get "/") . pure $ Redirect "/folio/state"
    folio
    SIS.api
