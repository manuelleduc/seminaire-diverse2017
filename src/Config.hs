{-# LANGUAGE OverloadedStrings #-}
module Config (parseConfig, Config(Config)) where

import qualified Data.Configurator as C
import qualified Data.Text         as T

data Config = Config {
  db   :: T.Text,
  port :: Int
}

parseConfig :: FilePath -> IO Config
parseConfig cfgFile =
    do cfg <- C.load [C.Required cfgFile]
       db <- C.require cfg "db"
       port <- C.require cfg "port"
       return (Config db port)
