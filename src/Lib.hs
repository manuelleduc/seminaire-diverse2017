{-# LANGUAGE DataKinds         #-}
{-{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)
import           Domain                   (Token (..), User (User))

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


-- TODO: save tokens as persistent
type API = "login" :> ReqBody '[JSON] User :> Post '[JSON] Token

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
server pool = loginPostH
  where loginPostH user = liftIO $ loginPost pool user

loginPost:: ConnectionPool -> User -> IO Token
loginPost pool (User "admin" "admin") = flip runSqlPersistMPool pool $ do
      t <- liftIO getCurrentTime
      r <- liftIO $ stringRandomIO "[a-zA-Z0-9]{50}"
      insert (D.Token r t)
      return $ Token (unpack r)
loginPost _ _ = return $ TokenError "invalid login and password"
