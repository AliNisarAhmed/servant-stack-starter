{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}


module Lib where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Data.Text as T
import           Model
import           Data.Time (UTCTime, getCurrentTime)
import           Control.Monad.IO.Class (liftIO)
import           Config
import           Network.Wai.Middleware.Cors
import           Database (runDb)
import           Database.Persist
import           Control.Monad.Reader (runReaderT)

import Network.Wai.Middleware.Servant.Options

app :: Config -> Application
app cfg =
  cors (const $ Just policy)
  $ provideOptions api
  $ serve api (appToServer cfg)
  where
    policy = CorsResourcePolicy {
      corsOrigins        = Nothing
    , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge         = Nothing
    , corsVaryOrigin     = False
    , corsRequireOrigin  = False
    , corsIgnoreFailures = False
    }

api :: Proxy API
api = Proxy


convertApp :: Config -> App a -> Handler a
convertApp cfg appt = runReaderT (runApp appt) cfg

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Server API
appToServer cfg = hoistServer api (convertApp cfg) server


---


type API =
  "api" :> "whoami" :> Get '[JSON] [Entity Question]

server :: ServerT API App
server = whoami

whoami :: App [Entity Question]
whoami = runDb $ selectList [] []

migrateDb :: ConnectionPool -> IO ()
migrateDb pool = runSqlPool (runMigration migrateAll) pool