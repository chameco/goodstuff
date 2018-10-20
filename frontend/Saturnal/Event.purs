module Saturnal.Event where

import Prelude
import Saturnal.Types

import Effect (Effect)

type Viewport = { r :: Number, x :: Number, y :: Number }
data State = State Viewport Board

foreign import _setState :: Unit -> State -> Effect Unit

setState :: State -> Effect Unit
setState = _setState unit

foreign import withState :: forall a. a -> (State -> a) -> Effect a

foreign import _listen :: Unit -> String -> String -> Effect Unit -> Effect Unit

listen :: String -> String -> Effect Unit -> Effect Unit
listen = _listen unit

foreign import _key :: Unit -> String -> Effect Unit -> Effect Unit

key :: String -> Effect Unit -> Effect Unit
key = _key unit

foreign import _frames :: Unit -> Effect Unit -> Effect Unit

frames :: Effect Unit -> Effect Unit
frames = _frames unit
