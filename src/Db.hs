{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Db where

import           Data.Aeson
import           Data.Text
import           Data.Time.Clock (UTCTime)

import           Database.Persist.TH

-- TODO: expiration time
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Token
  tokenValue Text
  timestamp  UTCTime
  deriving Eq Read Show
|]


instance FromJSON Token where
  parseJSON = withObject "Token" $ \ v ->
    Token <$> v .: "tokenValue"
          <*> v .: "timestamp"


instance ToJSON Token where
  toJSON (Token tokenValue timestamp) =
    object [
      "tokenValue" .= tokenValue
    , "timestamp" .= timestamp
    ]
