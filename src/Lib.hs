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
import Model
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.IO.Class (liftIO)

port = 8080

startApp :: IO ()
startApp =
  putStrLn ("server running on port: " ++ show port) >> run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

---


type API =
  "api" :> "whoami" :> Get '[JSON] Question

server :: Server API
server = whoami

whoami :: Handler Question
whoami = do
  now <- liftIO getCurrentTime
  return $ q now

q time = Question "Title" "Content" time 2