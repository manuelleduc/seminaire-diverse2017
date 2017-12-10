{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Db where

import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Logger
import           Control.Monad.Reader         (ReaderT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Text
import           Data.Time.Clock              (UTCTime, addUTCTime)
import           Database.Persist             ((<.), (==.))
import           Database.Persist.Class       (BaseBackend, PersistQueryRead,
                                               PersistQueryWrite,
                                               PersistRecordBackend)
import           Database.Persist.Sql         (SqlPersistT, runSqlConn)
import           Database.Persist.Sqlite      (SqlBackend, SqlPersistM,
                                               deleteWhere, insert, selectFirst)
import           Database.Persist.TH
import           Database.Persist.Types       (Entity, SelectOpt)
import           Web.Spock

-- TODO: expiration time
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Token
  tokenValue Text
  timestamp  UTCTime
  deriving Eq Read Show
|]


instance FromJSON Token where
  parseJSON = withObject "Token" $ \ v ->
    Token <$> v .: "tokenValue"
          <*> v .: "timestamp"


instance ToJSON Token where
  toJSON (Token tokenValue timestamp) =
    object [
      "tokenValue" .= tokenValue
    , "timestamp" .= timestamp
    ]

createToken :: Text -> UTCTime -> SqlPersistM TokenId
createToken token currentTime = insert (Token token currentTime)

removeOldTokens :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryWrite backend) => UTCTime -> ReaderT backend m ()
removeOldTokens currentTime = deleteWhere [TokenTimestamp <. addUTCTime (-900000) currentTime]

findToken :: (BaseBackend backend ~ SqlBackend, PersistQueryRead backend, MonadIO m) => Text -> ReaderT backend m (Maybe (Entity Token))
findToken token = selectFirst [TokenTokenValue ==. token] []

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}
