-- Code taken from tutorial example that can be found here https://github.com/slamdata/purescript-halogen
module CounterExample (runApp) where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

newtype State = State Int

initialState :: State
initialState = State 0

data Query a = Tick a

ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render (State n) =
    H.div_
      [ H.h1
          [ P.id_ "header" ]
          [ H.text "counter" ]
      , H.p_
          [ H.text (show n) ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (Tick next) = do
    modify (\(State n) -> State (n + 1))
    pure next

-- | Run the app
runApp :: Eff (HalogenEffects ()) Unit
runApp = runAff throwException (const (pure unit)) $ do
  { node: node, driver: driver } <- runUI ui initialState
  onLoad $ appendToBody node
  setInterval 1000 $ driver (action Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a

{--runApp :: forall eff. Eff eff Unit--}
{--runApp = unsafeThrow ""--}
