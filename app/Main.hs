module Main where

import Good.Prelude

import System.Environment (getEnv)

import Good.Interfaces.Web
import qualified Good.Services.SIS as SIS
import qualified Good.Services.IED as IED

main :: IO ()
main = do (Just port) <- readMay <$> getEnv "PORT"
          serving port $ do
            handling (Get "/") . pure $ Redirect "/ied/state"
            SIS.api
            IED.api
