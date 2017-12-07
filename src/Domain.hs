{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Domain (User(User),Token(Token,TokenError)) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data User = User
  { login :: String
  , pass  :: String
  } deriving (Eq, Show)


data Token = Token {
  tokenValue :: String
} | TokenError {
  fault :: String
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Token)
