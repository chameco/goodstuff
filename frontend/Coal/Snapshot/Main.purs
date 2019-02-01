module Coal.Snapshot.Main where

import Coal.Snapshot.Event (listen)
import Coal.Snapshot.IotM (buildIotM)
import Coal.Snapshot.Net (getInfo, getSnapshot, retrieveInfo, retrieveSnapshot, setInfo, setSnapshot, submitSnapshot, unsetInfo, unsetSnapshot)
import Coal.Snapshot.Types (Info(..), Snapshot(..), opts)
import Coal.Snapshot.UI (display, getValue, setHTML, setValue, undisplay)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Foldable (fold, foldr)
import Data.Function (flip, ($))
import Data.Functor (map, (<$>))
import Data.Maybe (Maybe(..))
import Data.Ord ((<=))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Sequence (Seq, empty, fromFoldable, length, null, singleton, splitAt)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (error)
import Foreign.Generic (genericDecodeJSON)

tabs :: Array String
tabs = ["profile", "iotm", "skills"]

switchDefaultTab :: Effect Unit
switchDefaultTab = do
  for_ tabs $ undisplay <<< flip (<>) "tab"
  display "uploadtab"

switchTab :: String -> Effect Unit
switchTab tab = do
  undisplay "uploadtab"
  for_ tabs $ undisplay <<< flip (<>) "tab"
  html <- buildTab tab
  setHTML (tab <> "tab") html
  display $ tab <> "tab"

buildTab :: String -> Effect String
buildTab "profile" = do
  mi <- getInfo
  case mi of
    Just (Info info) ->
      pure $ fold [ "<div id=\"profileinfo\">"
                  , "<div id=\"profilename\">", info.name, "</div>"
                  , "<div id=\"profiletitle\">", info.title, "</div>"
                  , "<img id=\"profileavatar\" src=\"", info.avatar, "\">"
                  , "</div>"
                  ]
    _ -> pure "No loaded player info."
buildTab "iotm" = do
  ms <- getSnapshot
  case ms of
    Just (Snapshot snapshot) ->
      pure $ fold [ "<div id=\"iotminfo\">"
                  , buildIotM $ fromFoldable snapshot.iotm
                  , "</div>"
                  ]
    _ -> pure "No loaded player snapshot."
buildTab _ = pure "Unknown tab. You're doing something strange!"

fetch :: String -> Effect Unit
fetch playerid = do
  unsetInfo
  unsetSnapshot
  retrieveInfo playerid $ \infoJSON ->
    case runExcept $ genericDecodeJSON opts infoJSON of
      Right info -> do
        setValue "playerid" playerid
        setInfo info
        retrieveSnapshot playerid $ \snapshotJSON ->
          case runExcept $ genericDecodeJSON opts snapshotJSON of
            Right snapshot -> do
              setSnapshot snapshot
            _ -> pure unit
        switchTab "profile"
      _ -> error "Could not retrieve player info."

main :: Effect Unit
main = do
  for_ tabs $ \tab -> listen tab "click" $ switchTab tab
  listen "upload" "click" switchDefaultTab
  listen "uploadsubmit" "click" $ do
    ms <- getValue "uploadtext"
    mp <- getValue "uploadplayerid"
    case Tuple ms mp of
      Tuple (Just snapshotJSON) (Just playerid) ->
        submitSnapshot playerid snapshotJSON $ do
          setValue "uploadtext" ""
          fetch playerid
      _ -> pure unit
  listen "snapshot" "click" $ do
    mp <- getValue "playerid"
    case mp of
      Just playerid -> fetch playerid
      _ -> pure unit
