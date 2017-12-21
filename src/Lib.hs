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
import Services

start :: Config -> IO ()
start config@(Config db port) =
    do pool <- runNoLoggingT $ createSqlitePool db 5
       runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
       spockCfg <- defaultSpockCfg () (PCPool pool) config
       runSpock port (spock spockCfg app)


app :: ApiApp ()
app =
    do post "login" postLogin
       post "compute" postCompute
