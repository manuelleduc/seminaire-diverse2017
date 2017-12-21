{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Services (postCompute, postLogin, ApiApp) where


import           Config
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Logger      (runNoLoggingT)
import           Control.Monad.Trans
import           Data.Aeson                hiding (json)
import           Data.Complex
import           Data.IORef
import           Data.Monoid
import           Data.Text                 (pack)
import qualified Data.Text                 as T
import           Data.Time.Clock           (getCurrentTime)
import           Database.Persist.Sqlite   (SqlBackend, createSqlitePool,
                                            runMigration, runSqlPool)
import           Db                        (createToken, findToken, migrateAll,
                                            removeOldTokens, runSQL)
import           Eval                      (eval)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types.Status
import           Text.Read                 (readMaybe)
import           Text.StringRandom         (stringRandomIO)
import           Web.Spock
import           Web.Spock.Config
import Data

type MySession = ()
type MyAppState = Config


type ApiAction ctx a = SpockActionCtx  ctx SqlBackend MySession MyAppState a
type ApiApp ctx = SpockCtxM ctx SqlBackend MySession MyAppState ()


postLogin :: ActionCtxT () (WebStateM SqlBackend MySession MyAppState) b
postLogin = do
  usr <- jsonBody' :: ApiAction () User
  case usr of
    User "admin" "admin" -> do
      currentTime <- liftIO getCurrentTime
      token <- liftIO $ stringRandomIO "[a-zA-Z0-9]{50}"
      runSQL $ createToken token currentTime
      json $ object ["token" .= String token]
    _ -> do setStatus status401
            json $ object ["fault" .= String "invalid login and password"]

postCompute :: ActionCtxT () (WebStateM SqlBackend MySession MyAppState) b
postCompute = do
  (ComputeData token expression) <- jsonBody' :: ApiAction () ComputeData
  currentTime <- liftIO getCurrentTime
  runSQL $ removeOldTokens currentTime -- cleanup old tokens
  tokenLine <- runSQL $ findToken (pack token)
  case tokenLine of
    Just _  -> do
      v <- liftIO $ eval expression
      case v of
        Left _ -> do setStatus status418
                     json $ object ["fault" .= String ( pack $ "can't parse expression [" ++ expression ++ "]")]
        Right a -> case a of
          "True" -> json $ object ["result" .= Bool True]
          "False" -> json $ object ["result" .= Bool False]
          _ -> case readMaybe a of
            Nothing -> do setStatus status418
                          json $ object ["fault" .= String ( pack $ "can't parse expression [" ++ expression ++ "]")]
            Just nv -> json $ object ["result" .= Number nv]
    Nothing -> do setStatus status401
                  json $ object ["fault" .= String "invalid token"]
