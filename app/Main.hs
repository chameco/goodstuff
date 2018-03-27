module Main where

import Good.Prelude

import System.Environment (getEnv)

import Good.Utilities.Web
import Good.API.SIS

main :: IO ()
main = do (Just port) <- readMay <$> getEnv "PORT"
          serving port sis
