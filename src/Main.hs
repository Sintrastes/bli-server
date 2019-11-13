{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.HTTP.Types
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as B
import Data.Text (pack)
import Data.Monoid
import Data.Text.Encoding
import Data.String
import Control.Applicative
import Servant
import Servant.API
import Data.Aeson
import GHC.Generics

type MyResponse = Bool

data JSONQuery = JSONQuery {
      query :: String
} deriving(Generic)

instance ToJSON JSONQuery

data Journal = Journal {
      content :: String
} deriving(Generic)

instance ToJSON Journal

instance FromJSON Journal

type BedelibryServerApi = 
      "prolog"  :> QueryParam "query" String :> Get '[JSON] MyResponse
 :<|> "journal" :> ReqBody '[JSON] Journal :> Post '[JSON] MyResponse
 :<|> "id" :> QueryParam "term" String :> Get '[JSON] String

server :: Server BedelibryServerApi
server = (\_ -> return True)
    :<|> (\(Journal content) -> return True)
    :<|> (\x -> do
            case x of 
              Just s -> return s
              Nothing -> return "Error")

userAPI :: Proxy BedelibryServerApi
userAPI = Proxy

app :: Application
app = serve userAPI server
-- app req respond = 
--     respond $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

main :: IO ()
main = do
  run 3000 app 
  putStrLn "Hello, Haskell!"
