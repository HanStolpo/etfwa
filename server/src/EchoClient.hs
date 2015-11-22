{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module EchoClient
    ( Api
    , server
    , runApp
    ) where


import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  as T
import           Data.Time.Clock
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp   hiding (Settings)
import           Servant
import           System.Directory

import qualified Client
import qualified Echo



type Api = Echo.Api
         :<|> Client.Api


api :: Proxy Api
api = Proxy

server :: Client.Settings -> Server Api
server s = Echo.server
         :<|> Client.server s


app :: Client.Settings -> Application
app s = serve api (server s)

runApp :: Int -> Text -> IO ()
runApp port clientApp = run port (app Client.Settings{Client.port = port, Client.host = "localhost", Client.clientApp = clientApp})
