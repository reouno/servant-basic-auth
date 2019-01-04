{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Authentication where

import Data.Aeson
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.API.BasicAuth (BasicAuthData(..))
import Servant.Server

newtype PrivateData = PrivateData
    { ssshhh :: Text
    } deriving (Eq, Generic, Show)

instance ToJSON PrivateData

newtype PublicData = PublicData
    { someData :: Text
    } deriving (Eq, Generic, Show)

instance ToJSON PublicData

newtype User = User
    { userName :: Text
    } deriving (Eq, Show)

type PublicAPI = Get '[JSON] [PublicData]

type PrivateAPI = Get '[JSON] PrivateData

type BasicAPI =
    "public" :> PublicAPI
    :<|> "private" :> BasicAuth "foo-realm" User :> PrivateAPI

api :: Proxy BasicAPI
api = Proxy

authCheck :: BasicAuthCheck User
authCheck =
    let
        check (BasicAuthData username password) =
            if username == "servant" && password == "server"
                then return (Authorized (User "servant"))
                else return Unauthorized
    in
        BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

server :: Server BasicAPI
server =
    publicAPIHandler
    :<|> privateAPIHandler
    where
        publicAPIHandler = return [PublicData "foo", PublicData "bar"]
        privateAPIHandler (user :: User) = return (PrivateData (userName user))

basicAuthMain :: IO ()
basicAuthMain = run 8080
    (serveWithContext api
                      basicAuthServerContext
                      server
    )
