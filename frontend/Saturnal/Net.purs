module Saturnal.Net where

import Prelude
import Saturnal.Types

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import _getPlayer :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)

getPlayer :: Effect (Maybe String)
getPlayer = _getPlayer Just Nothing

foreign import _setGame :: Unit -> String -> Effect Unit

setGame :: String -> Effect Unit
setGame = _setGame unit

foreign import _getGame :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)

getGame :: Effect (Maybe String)
getGame = _getGame Just Nothing

foreign import _login :: Unit -> String -> String -> Effect Unit -> Effect Unit

login :: String -> String -> Effect Unit -> Effect Unit
login = _login unit

foreign import _poll :: Unit -> String -> (String -> Effect Unit) -> Effect Unit

poll :: String -> (String -> Effect Unit) -> Effect Unit
poll = _poll unit

foreign import _submitTurn :: Unit -> String -> Effect Unit -> Effect Unit

submitTurn :: String -> Effect Unit -> Effect Unit
submitTurn = _submitTurn unit
