{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Echo
    ( Api
    , server
    , runApp
    ) where


import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Functor
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  as T
import           Data.Time.Clock
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory

data EchoMessage = EchoMessage
  { path      :: Text
  , message   :: Text
  , timeStamp :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON EchoMessage
instance FromJSON EchoMessage

type Api = "echo" :> "path" :> Get '[JSON] EchoMessage
         :<|> "echo" :> Capture "hello" Text :> QueryParam "message" Text :> Get '[JSON] EchoMessage
         :<|> "echo" :> QueryParam "message" Text :> Get '[JSON] EchoMessage


echoHello :: Text -> Maybe Text -> EitherT ServantErr IO EchoMessage
echoHello p m = EchoMessage ("echo/" <> p) ("hello your message was \"" <> fromMaybe "" m <> "\"") <$> liftIO getCurrentTime

echo :: Maybe Text -> EitherT ServantErr IO EchoMessage
echo m = EchoMessage "echo" (fromMaybe "" m) <$> liftIO getCurrentTime

echoPath :: EitherT ServantErr IO EchoMessage
echoPath = EchoMessage "echo/path" <$> (T.pack <$> liftIO getCurrentDirectory) <*> liftIO getCurrentTime

api :: Proxy Api
api = Proxy

server :: Server Api
server = echoPath :<|> echoHello :<|> echo

app :: Application
app = serve api server

runApp :: Int -> IO ()
runApp port = run port app
