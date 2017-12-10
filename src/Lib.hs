{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Lib where

import           Config
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger      (runNoLoggingT)
import           Control.Monad.Trans
import           Data.Aeson                hiding (json)
import           Data.IORef
import           Data.Monoid
import qualified Data.Text                 as T
import           Database.Persist.Sqlite   (createSqlitePool, runMigration,
                                            runSqlPool)
import           Db                        (migrateAll)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Status
import           Text.StringRandom         (stringRandomIO)
import           Web.Spock
import           Web.Spock.Config

start :: Config -> IO ()
start (Config db port) =
    do pool <- runNoLoggingT $ createSqlitePool db 5
       runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
       ref <- newIORef 0
       spockCfg <- defaultSpockCfg () PCNoDatabase (DummyAppState ref)
       runSpock port (spock spockCfg app)

type MySession = ()
newtype MyAppState = DummyAppState (IORef Int)

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

type ApiAction a = SpockAction  () MySession MyAppState a


app :: SpockM () MySession MyAppState ()
app =
    do post "login" $ do
         usr <- jsonBody' :: ApiAction User
         case usr of
           User "admin" "admin" -> do
             token <- liftIO $ stringRandomIO "[a-zA-Z0-9]{50}"
             json $ object ["token" .= String token]
           _ -> do setStatus status401
                   json $ object ["fault" .= String "invalid login and password"]
       post "compute" $ do
         rq <- jsonBody' :: ApiAction ComputeData
         json $ object  ["result" .= Number 4.2]
