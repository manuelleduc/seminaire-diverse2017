{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Domain (User(User)
  , AuthError(AuthError)
  , Expression(Expression)
  , Result(Result)
) where

import           Data.Aeson               hiding (Result)
import           Data.Aeson.TH
import           GHC.Generics             (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data User = User
  { login :: String
  , pass  :: String
  } deriving (Eq, Show)

newtype AuthError = AuthError { fault :: String }
  deriving (Show, Generic, Eq, Ord)

data Expression = Expression {
  token      :: String,
  expression :: String
} deriving (Eq, Show)

newtype Result = Result {
  result:: Double
} deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''AuthError)
$(deriveJSON defaultOptions ''Expression)
$(deriveJSON defaultOptions ''Result)
