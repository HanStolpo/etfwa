-- Code taken from tutorial example that can be found here http://www.parsonsmatt.org/programming/2015/10/22/purescript_router.html
module RoutesExample (runApp) where

import Prelude
import Control.Monad.Aff (runAff, forkAff)

import DOM
import Halogen
import Halogen.HTML.Elements (div_)
{--import Halogen.HTML.Elements as H--}
import Halogen.HTML as H
import Halogen.Util (appendToBody)
import Routing
import Routing.Match
import Routing.Match.Class
import Data.Functor
import Data.Tuple
import Data.Maybe
import Control.Monad.Eff.Exception
import Control.Monad.Eff
import Control.Apply
import Control.Alt
import Control.Monad.Aff.AVar
import Control.Monad.Aff hiding (liftEff')


data Routes
  = Profile
  | Sessions
  | Home

routing :: Match Routes
routing = Profile <$ lit "" <* lit "profile"
      <|> Sessions <$ lit "" <* lit "sessions"
      <|> Home <$ lit ""

type State = { currentPage :: String }

init :: State
init = {currentPage: "blah"}

data Input a
  = Goto Routes a

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render st =
      div_
        [ H.h1_ [ H.text (st.currentPage) ]
        , H.p_ [ H.text "Routing!!" ]
        ]

    eval :: Eval Input State Input g
    eval (Goto Sessions next) = do
      modify (_{ currentPage = "Sessions" })
      pure next
    eval (Goto Home next) = do
      modify (_{ currentPage = "Home" })
      pure next
    eval (Goto Profile next) = do
      modify (_{ currentPage = "Profile" })
      pure next

type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver Input eff
            -> Aff (Effects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. Driver Input eff
          -> Maybe Routes
          -> Routes
          -> Aff (Effects eff) Unit
redirects driver _ Sessions = do
  driver (action (Goto Sessions))
redirects driver _ Profile = do
  driver (action (Goto Profile))
redirects driver _ Home = do
  driver (action (Goto Home))

runApp :: forall eff. Eff (Effects eff) Unit
runApp = runAff throwException (const (pure unit)) $ do
  app <- runUI ui init
  appendToBody app.node
  forkAff $ routeSignal app.driver

