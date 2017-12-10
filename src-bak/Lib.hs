{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger       (runStderrLoggingT)
import qualified Domain                     as Do (AuthError (AuthError),
                                                   Expression (Expression),
                                                   Result (Result), User (User))

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp   as Warp

import           Servant

import           Data.Text
import qualified Db

import           Data.Time.Clock            (getCurrentTime)

import           Text.StringRandom          (stringRandomIO)

import           Control.Monad.Error.Class  (MonadError)

import qualified Data.ByteString.Lazy.Char8 as BS (pack)

import           Control.Monad              ((<=<))

type API = "login"   :> ReqBody '[JSON]  Do.User       :> Post '[JSON] Db.Token
       :<|>"compute" :> ReqBody '[JSON]' Do.Expression :> Post '[JSON] Do.Result

startApp :: FilePath ->  IO ()
startApp sqliteFile = run 8080 =<< mkApp sqliteFile

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration Db.migrateAll) pool
  return $ app pool

app :: ConnectionPool -> Application
app pool = serve api $ server pool

api :: Proxy API
api = Proxy

server :: ConnectionPool -> Server API
server pool = loginPostH pool :<|> computePostH pool

loginPostH ::  ConnectionPool -> Do.User -> Handler Db.Token
loginPostH pool = withError <=< liftIO . loginPost pool

computePostH :: ConnectionPool -> Do.Expression -> Handler Do.Result
computePostH pool = withError <=< liftIO . computePost pool

withError :: MonadError ServantErr m => Either Do.AuthError a -> m a
withError (Right a)            = return a
withError (Left (Do.AuthError t)) = throwError $ err401 { errBody = BS.pack t }

computePost:: ConnectionPool -> Do.Expression -> IO (Either Do.AuthError Do.Result)
computePost pool _ = flip runSqlPersistMPool pool $ do
  return $ Right (Do.Result 1.0)

loginPost:: ConnectionPool -> Do.User -> IO (Either Do.AuthError Db.Token)
loginPost pool (Do.User "admin" "admin") = flip runSqlPersistMPool pool $ do
      t <- liftIO getCurrentTime
      r <- liftIO $ stringRandomIO "[a-zA-Z0-9]{50}"
      let token = Db.Token r t
      insert token
      return $ Right token
loginPost _ _ = return $ Left (Do.AuthError "invalid login and password") -- $ err401 undefined --return $ TokenError "invalid login and password"
