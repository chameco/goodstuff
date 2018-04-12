module Main where

import Good.Prelude

import System.Environment (getEnv)

import Good.Interfaces.Web
import qualified Good.Services.SIS as SIS

main :: IO ()
main = do (Just port) <- readMay <$> getEnv "PORT"
          serving port SIS.api
