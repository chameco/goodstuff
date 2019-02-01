module Coal.Snapshot.Event where

import Data.Unit (Unit, unit)
import Effect (Effect)

foreign import _listen :: Unit -> String -> String -> Effect Unit -> Effect Unit

listen :: String -> String -> Effect Unit -> Effect Unit
listen = _listen unit

foreign import _after :: Unit -> Number -> Effect Unit -> Effect Unit

after :: Number -> Effect Unit -> Effect Unit
after = _after unit
