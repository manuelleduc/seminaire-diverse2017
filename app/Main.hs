module Main where

import           Lib
import Config

main :: IO ()
main = do
  cfg <- parseConfig "server.cfg"
  start cfg
