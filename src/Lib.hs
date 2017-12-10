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
import           Data.Complex
import           Data.IORef
import           Data.Map
import           Data.Monoid
import           Data.Scientific           (Scientific, toRealFloat)
import           Data.Text                 (pack)
import qualified Data.Text                 as T
import           Data.Time.Clock           (getCurrentTime)
import           Database.Persist.Sqlite   (SqlBackend, createSqlitePool,
                                            runMigration, runSqlPool)
import           Db                        (createToken, findToken, migrateAll,
                                            removeOldTokens, runSQL)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Status
import           Text.Parsec.Expr.Math
import           Text.StringRandom         (stringRandomIO)
import           Web.Spock
import           Web.Spock.Config

start :: Config -> IO ()
start config@(Config db port) =
    do pool <- runNoLoggingT $ createSqlitePool db 5
       runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
       spockCfg <- defaultSpockCfg () (PCPool pool) (DummyAppState config)
       runSpock port (spock spockCfg app)

type MySession = ()
newtype MyAppState = DummyAppState Config

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

type ApiAction ctx a = SpockActionCtx  ctx SqlBackend MySession MyAppState a
type ApiApp ctx = SpockCtxM ctx SqlBackend MySession MyAppState ()

app :: ApiApp ()
app =
    do post "login" $ do
         usr <- jsonBody' :: ApiAction () User
         case usr of
           User "admin" "admin" -> do
             currentTime <- liftIO getCurrentTime
             token <- liftIO $ stringRandomIO "[a-zA-Z0-9]{50}"
             runSQL $ createToken token currentTime
             json $ object ["token" .= String token]
           _ -> do setStatus status401
                   json $ object ["fault" .= String "invalid login and password"]
       post "compute" $ do
         (ComputeData token expression) <- jsonBody' :: ApiAction () ComputeData
         currentTime <- liftIO getCurrentTime
         runSQL $ removeOldTokens currentTime -- cleanup old tokens
         tokenLine <- runSQL $ findToken (pack token)
         case tokenLine of
           Just _  -> case parse expression of
             Left _ -> do setStatus status418
                          json $ object ["fault" .= String (pack $ "can't parse the expression " ++  expression)]
             Right e -> case evaluate (fromList []) . Just $ e of
               Just a -> json $ object ["result" .= String (pack . show $ a)] -- todo print a Number if it can be represented as such, and same thing with booleans
               Nothing -> do setStatus status418
                             json $ object ["fault" .= String (pack $ "can't parse the expression 2 " ++ expression)]

           Nothing -> do setStatus status401
                         json $ object ["fault" .= String "invalid token"]
