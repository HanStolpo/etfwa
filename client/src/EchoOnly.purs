module EchoOnly (runApp) where

import Prelude
import Control.Monad.Eff
import Data.Maybe.Unsafe (unsafeThrow)

runApp :: forall eff. Eff eff Unit
runApp = unsafeThrow ""
