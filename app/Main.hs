module Main where

import Good.Prelude

import Good.Utilities.Web
import Good.API.SIS

main :: IO ()
main = serving 3000 sis
