module Saturnal.Net where

import Control.Bind (bind, pure)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.String (drop, null)
import Data.Unit (Unit, unit)
import Effect (Effect)

foreign import _getPlayerCookie :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)
getPlayerCookie :: Effect (Maybe String)
getPlayerCookie = _getPlayerCookie Just Nothing

foreign import _getGameURL :: Effect String
getGameURL :: Effect (Maybe String)
getGameURL = do
  game <- drop 1 <$> _getGameURL
  if null game then pure Nothing else pure $ Just game

foreign import _getPlayer :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)
getPlayer :: Effect (Maybe String)
getPlayer = _getPlayer Just Nothing

foreign import _setPlayer :: Unit -> String -> Effect Unit
setPlayer :: String -> Effect Unit
setPlayer = _setPlayer unit

foreign import _getGame :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe String)
getGame :: Effect (Maybe String)
getGame = _getGame Just Nothing

foreign import _setGame :: Unit -> String -> Effect Unit
setGame :: String -> Effect Unit
setGame = _setGame unit

foreign import _login :: Unit -> String -> String -> Effect Unit -> Effect Unit
login :: String -> String -> Effect Unit -> Effect Unit
login = _login unit

foreign import _newGame :: Unit -> String -> String -> (String -> Effect Unit) -> Effect Unit
newGame :: String -> String -> (String -> Effect Unit) -> Effect Unit
newGame = _newGame unit

foreign import _invite :: Unit -> String -> Effect Unit -> Effect Unit
invite :: String -> Effect Unit -> Effect Unit
invite = _invite unit

foreign import _poll :: Unit -> String -> (String -> Effect Unit) -> Effect Unit
poll :: String -> (String -> Effect Unit) -> Effect Unit
poll = _poll unit

foreign import _submitTurn :: Unit -> String -> Effect Unit -> Effect Unit
submitTurn :: String -> Effect Unit -> Effect Unit
submitTurn = _submitTurn unit
