module EchoOnly (runApp) where

import Prelude

import Control.Apply
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Console hiding (log, print)
import qualified Network.HTTP.Affjax as Ajax
import Network.HTTP.MimeType.Common (applicationJSON)
import Network.HTTP.Affjax.Response
import Network.HTTP.MimeType
import Data.Foreign.Class

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E
-- import qualified Module Halogen.HTML.Events.Forms as E

import Data.Maybe
import Data.Maybe.Unsafe (unsafeThrow)
import Data.Tuple
import Debug.Trace

type EchoResponse =
  { path      :: String
  , message   :: String
  , timeStamp :: String
  }



data MessageType
  = HelloMessage {message :: String, response :: Maybe EchoResponse}
  | EchoMessage {message :: String, response :: Maybe EchoResponse}
  | PathMessage {response :: Maybe EchoResponse}

type State = MessageType
-- newtype State = State Int

initialState :: State
initialState = HelloMessage {message: "", response: Nothing}

type Effects eff = HalogenEffects (console :: CONSOLE, ajax :: Ajax.AJAX | eff)

data Query a
  = SelectHello a
  | SelectEcho a
  | SelectPath a
  | SetHello String a
  | SetEcho String a
  | SendHello State a
  | SendEcho State a
  | SendPath State a

ui :: forall eff. Component State Query (Aff (Effects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render s = case renderOption s of
      Tuple option child ->
           H.div_
             [ H.select [E.onValueChange (E.input selectType)]
                 [ H.option [P.value "Hello", P.selected (option == "Hello")] [H.text "Hello"]
                 , H.option [P.value "Echo", P.selected (option == "Echo")] [H.text "Echo"]
                 , H.option [P.value "Path", P.selected (option == "Path")] [H.text "Path"]
                 ]
             , child
             ]
      _ -> unsafeThrow "Never"

  selectType s
     | s == "Hello" = SelectHello
     | s == "Echo" = SelectEcho
     | s == "Path" = SelectPath

  renderOption s = case s of
     HelloMessage {message: m, response: r} -> Tuple "Hello" $ H.div_
        [ H.div_
            [ H.label_ [H.text "message"]
            , H.input [P.inputType P.InputText, E.onValueChange (E.input SetHello)]
            ]
        , H.div_
            [ H.label_ [H.text ("Hello response was:" ++ fromMaybe "" (_.message <$> r))]
            ]
        , H.div_
            [ H.button [E.onClick (E.input_ $ SendHello s)] [H.text "echo friendly"]
            ]
        ]
     EchoMessage {message: m, response: r} -> Tuple "Echo" $ H.div_
        [ H.div_
            [ H.label_ [H.text "message"]
            , H.input [P.inputType P.InputText, E.onValueChange (E.input SetEcho)]
            ]
        , H.div_
            [ H.label_ [H.text ("Response was:" ++ fromMaybe "" (_.message <$> r))]
            ]
        , H.div_
            [ H.button [E.onClick (E.input_ $ SendEcho s)] [H.text "echo"]
            ]
        ]
     PathMessage {response: r} -> Tuple "Path" $ H.div_
        [ H.div_
            [ H.label_ [H.text ("Server path is:" ++ fromMaybe "" (_.message <$> r))]
            ]
        , H.div_
            [ H.button [E.onClick (E.input_ $ SendPath s)] [H.text "request path"]
            ]
        ]

  eval :: Natural Query (ComponentDSL State Query (Aff (Effects eff)))
  eval (SelectHello next) = pure next <* modify \_ -> (HelloMessage {message: "", response: Nothing})
  eval (SetHello m next) = pure next <* modify
    \state -> case state of
      HelloMessage d -> HelloMessage d{message=m, response=Nothing}
      _ -> state
  eval (SendHello s next) = pure next <* case s of
    HelloMessage d -> do
        r <- liftAff' $ sendHello d.message
        modify \_ -> HelloMessage d{response = Just r}
    _ -> unsafeThrow "Never"

  eval (SelectEcho next) = modify (\state -> EchoMessage {message: "", response: Nothing}) *> pure next
  eval (SetEcho m next) = pure next <* modify
    \state -> case state of
        EchoMessage d -> EchoMessage d{message=m, response=Nothing}
        _ -> state
  eval (SendEcho s next) = pure next <* case s of
    EchoMessage d -> do
        r <- liftAff' $ sendEcho d.message
        modify \_ -> EchoMessage d{response = Just r}
    _ -> unsafeThrow "Never"

  eval (SelectPath next) = modify (\state -> PathMessage {response: Nothing}) *> pure next
  eval (SendPath s next) = pure next <* case s of
    PathMessage d -> do
        r <- liftAff' $ sendPath
        modify \_ -> PathMessage d{response = Just r}
    _ -> unsafeThrow "Never"

newtype EchoResponseR = EchoResponseR EchoResponse
instance respondableEchoResponseR :: Respondable EchoResponseR where
  fromResponse r' = do
    r <- read r' >>= readJSON  -- response is a text string apparently so first read it then turn string into JSON object for the rest of the parsing
    p <- readProp "path" r
    m <- readProp "message" r
    t <- readProp "timeStamp" r
    pure $ EchoResponseR {path: p, message: m, timeStamp: t}
  responseType = Tuple (Just applicationJSON) JSONResponse

sendHello :: forall eff. String -> Aff (ajax :: Ajax.AJAX | eff) EchoResponse
sendHello s =  Ajax.get ("http://localhost:8086/echo/hello?message=" ++ s) <#> \a -> case a.response of EchoResponseR r -> r

sendEcho :: forall eff. String -> Aff (ajax :: Ajax.AJAX | eff) EchoResponse
sendEcho s =  Ajax.get ("http://localhost:8086/echo?message=" ++ s) <#> \a -> case a.response of EchoResponseR r -> r

sendPath :: forall eff. Aff (ajax :: Ajax.AJAX | eff) EchoResponse
sendPath =  Ajax.get ("http://localhost:8086/echo/path") <#> \a -> case a.response of EchoResponseR r -> r


-- | Run the app
runApp :: Eff (Effects ()) Unit
runApp = runAff throwException (const (pure unit)) $ do
  { node: node, driver: driver } <- runUI ui initialState
  onLoad $ appendToBody node
