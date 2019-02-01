module Coal.Snapshot.UI where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Effect (Effect)

foreign import _toggle :: Unit -> String -> Effect Unit
toggle :: String -> Effect Unit
toggle = _toggle unit

foreign import _hide :: Unit -> String -> Effect Unit
hide :: String -> Effect Unit
hide = _hide unit

foreign import _unhide :: Unit -> String -> Effect Unit
unhide :: String -> Effect Unit
unhide = _unhide unit

foreign import _display :: Unit -> String -> Effect Unit
display :: String -> Effect Unit
display = _display unit

foreign import _undisplay :: Unit -> String -> Effect Unit
undisplay :: String -> Effect Unit
undisplay = _undisplay unit

foreign import _setHTML :: Unit -> String -> String -> Effect Unit
setHTML :: String -> String -> Effect Unit
setHTML = _setHTML unit

foreign import _getValue :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> String -> Effect (Maybe String)
getValue :: String -> Effect (Maybe String)
getValue = _getValue Just Nothing

foreign import _setValue :: Unit -> String -> String -> Effect Unit
setValue :: String -> String -> Effect Unit
setValue = _setValue unit
