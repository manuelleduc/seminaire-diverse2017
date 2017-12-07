{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    , app
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Domain                   (Token(Token), User(User))


-- TODO: save tokens as persistent
type API = "users" :> Get '[JSON] [User]
  :<|> "login" :> ReqBody '[JSON] User :> Post '[JSON] Token

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
  :<|> loginPost

users :: [User]
users = [ User "admin" "admin"]

loginPost:: User -> Handler Token
loginPost (User "admin" "admin") = return $ Token "yolo"
loginPost _ = return $ err503 { errBody = "invalid login and password" }
