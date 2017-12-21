{-# LANGUAGE DeriveGeneric       #-}

module Data where

import           Data.Aeson   hiding (json)
import           GHC.Generics (Generic)

data User = User {
  login:: String,
  pass  :: String
} deriving (Generic, Show)

data ComputeData = ComputeData  {
  token      :: String,
  expression :: String
} deriving (Generic, Show)

instance ToJSON User
instance FromJSON User

instance ToJSON ComputeData
instance FromJSON ComputeData
