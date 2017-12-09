{-# LANGUAGE DataKinds         #-}
{-{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)
import           Domain                   (User (User), AuthError(AuthError),Expression(Expression), Result(Result))

import           Data.String.Conversions

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Servant

import           Data.Text
import qualified Db                       as D

import           Data.Time.Clock          (getCurrentTime)

import           Text.StringRandom        (stringRandomIO)

import Control.Monad.Error.Class (MonadError)

import qualified Data.ByteString.Lazy.Char8 as BS (pack)

import Control.Monad ((<=<))

type API = {-"login" :> ReqBody '[JSON] User :> Post '[JSON] D.Token
  :<|>-} "compute" :> ReqBody '[JSON]' Expression :> Post '[JSON] Result

startApp :: FilePath ->  IO ()
startApp sqliteFile = run 8080 =<< mkApp sqliteFile

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration D.migrateAll) pool
  return $ app pool

app :: ConnectionPool -> Application
app pool = serve api $ server pool

api :: Proxy API
api = Proxy

server :: ConnectionPool -> Server API
server pool =  {-loginPostH :<|>-} computePostH
  where
    --loginPostH = withError <=< liftIO . loginPost pool
    computePostH = withError <=< (liftIO . computePost pool)

withError :: MonadError ServantErr m => Either AuthError a -> m a
withError (Right a) = return a
withError (Left (AuthError t)) = throwError $ err401 { errBody = BS.pack t }

computePost:: ConnectionPool -> Expression -> IO (Either AuthError Result)
computePost pool _ = flip runSqlPersistMPool pool $ do
  return $ Right (Result 1.0)

loginPost:: ConnectionPool -> User -> IO (Either AuthError D.Token)
loginPost pool (User "admin" "admin") = flip runSqlPersistMPool pool $ do
      t <- liftIO getCurrentTime
      r <- liftIO $ stringRandomIO "[a-zA-Z0-9]{50}"
      let token = D.Token r t
      insert token
      return $ Right token
loginPost _ _ = return $ Left (AuthError "invalid login and password") -- $ err401 undefined --return $ TokenError "invalid login and password"
