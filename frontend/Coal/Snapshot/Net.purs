module Coal.Snapshot.Net where

import Coal.Snapshot.Types (Info, Snapshot)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Effect (Effect)

foreign import _setSnapshot :: Unit -> Snapshot -> Effect Unit
setSnapshot :: Snapshot -> Effect Unit
setSnapshot = _setSnapshot unit

foreign import _unsetSnapshot :: Unit -> Effect Unit
unsetSnapshot :: Effect Unit
unsetSnapshot = _unsetSnapshot unit

foreign import _getSnapshot :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe Snapshot)
getSnapshot :: Effect (Maybe Snapshot)
getSnapshot = _getSnapshot Just Nothing

foreign import _retrieveSnapshot :: Unit -> String -> (String -> Effect Unit) -> Effect Unit
retrieveSnapshot :: String -> (String -> Effect Unit) -> Effect Unit
retrieveSnapshot = _retrieveSnapshot unit

foreign import _submitSnapshot :: Unit -> String -> String -> Effect Unit -> Effect Unit
submitSnapshot :: String -> String -> Effect Unit -> Effect Unit
submitSnapshot = _submitSnapshot unit

foreign import _setInfo :: Unit -> Info -> Effect Unit
setInfo :: Info -> Effect Unit
setInfo = _setInfo unit

foreign import _unsetInfo :: Unit -> Effect Unit
unsetInfo :: Effect Unit
unsetInfo = _unsetInfo unit

foreign import _getInfo :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Effect (Maybe Info)
getInfo :: Effect (Maybe Info)
getInfo = _getInfo Just Nothing

foreign import _retrieveInfo :: Unit -> String -> (String -> Effect Unit) -> Effect Unit
retrieveInfo :: String -> (String -> Effect Unit) -> Effect Unit
retrieveInfo = _retrieveInfo unit
