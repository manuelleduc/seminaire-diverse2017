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

import           Database.Persist.TH

-- TODO: expiration time
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Token
  tokenValue Text
  deriving Eq Read Show
|]


instance FromJSON Token where
  parseJSON = withObject "Token" $ \ v ->
    Token <$> v .: "tokenValue"


instance ToJSON Token where
  toJSON (Token tokenValue) =
    object [ "tokenValue" .= tokenValue ]
