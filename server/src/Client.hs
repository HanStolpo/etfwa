{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Client
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



type Api = "settings.js" :> Get '[PlainText] Text
         :<|> Raw

data Settings = Settings
  { port :: Int
  , host :: Text
  } deriving (Show)

handlerToServant :: Settings -> Handler :~> EitherT ServantErr IO
handlerToServant = runReaderTNat

type Handler = ReaderT Settings (EitherT ServantErr IO)

settings :: Handler Text
settings = do
  Settings {..} <- ask
  return $ T.unlines
    [ "SettingsETFWA = {};"
    , "SettingsETFWA.port = " <> (T.pack . show $ port) <> ";"
    , "SettingsETFWA.host = '" <> host <> "';"
    ]

content :: Server Raw
content = serveDirectory "../data"


api :: Proxy Api
api = Proxy

server :: Settings -> Server Api
server s = enter (handlerToServant s) settings
         :<|> content

app :: Settings -> Application
app s = serve api (server s)

runApp :: Int -> IO ()
runApp port = run port (app Settings{port = port, host = "localhost"})
