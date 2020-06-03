{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings   #-}


module Lib
  ( startApp
  , app
  )
where

import GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Data.Text as T

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)


port = 8080

startApp :: IO ()
startApp =
  putStrLn ("server running on port: " ++ show port) >> run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

---

data UserInfo = UserInfo
  { ipaddress :: T.Text
  , language :: T.Text
  , software :: T.Text
  } deriving (Generic, Eq, Show, ToJSON)


type API =
  "api" :> "whoami"
    :> Header "X-Forwarded-For" T.Text
    :> Header "User-Agent" T.Text
    :> Header "Accept-Language" T.Text
    :> Get '[JSON] UserInfo

server :: Server API
server = whoami

whoami :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Handler UserInfo
whoami ip ua lang = do
  let ip' = convert ip
      ua' = convert ua
      lang' = convert lang
  return $ UserInfo ip' ua' lang'
    where
      convert (Just v)  = v
      convert Nothing   = "unknown"