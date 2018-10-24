module Saturnal.Event where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Effect (Effect)

foreign import _listen :: Unit -> String -> String -> Effect Unit -> Effect Unit

listen :: String -> String -> Effect Unit -> Effect Unit
listen = _listen unit

foreign import _key :: Unit -> String -> Effect Unit -> Effect Unit

key :: String -> Effect Unit -> Effect Unit
key = _key unit

foreign import _mousedown :: Unit -> String -> (Number -> Number -> Effect Unit) -> Effect Unit

mousedown :: String -> (Number -> Number -> Effect Unit) -> Effect Unit
mousedown = _mousedown unit

foreign import _frames :: Unit -> Effect Unit -> Effect Unit

frames :: Effect Unit -> Effect Unit
frames = _frames unit

foreign import _after :: Unit -> Number -> Effect Unit -> Effect Unit

after :: Number -> Effect Unit -> Effect Unit
after = _after unit

foreign import _resize :: Unit -> Effect Unit -> Effect Unit

resize :: Effect Unit -> Effect Unit
resize = _resize unit
