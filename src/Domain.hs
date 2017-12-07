{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Domain (User(User),Token(Token)) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data User = User
  { login :: String
  , pass  :: String
  } deriving (Eq, Show)

newtype Token = Token {
  tokenValue :: String
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Token)
