{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Domain (User(User), AuthError(AuthError)) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import GHC.Generics (Generic)

data User = User
  { login :: String
  , pass  :: String
  } deriving (Eq, Show)

newtype AuthError = AuthError { fault :: String }
  deriving (Show, Generic, Eq, Ord)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''AuthError)
