module Coal.Snapshot.Main where

import Coal.Snapshot.Event (listen)
import Coal.Snapshot.IotM (buildIotM)
import Coal.Snapshot.Net (getInfo, getSnapshot, retrieveInfo, retrieveSnapshot, setInfo, setSnapshot, submitSnapshot, unsetInfo, unsetSnapshot)
import Coal.Snapshot.Skills (buildAirportSkills, buildAscensionSkills, buildClassSkills, buildCrimboSkills, buildDailyDungeonSkills, buildDeckSkills, buildDisSkills, buildDreadSkills, buildEldritchSkills, buildGingerbreadSkills, buildGnomeSkills, buildHobopolisSkills, buildLTTSkills, buildMimeSkills, buildMiscSkills, buildPVPSkills, buildPartySkills, buildRaffleSkills, buildSlimeSkills, buildSnojoSkills, buildSpacegateSkills, buildSpookyravenSkills, buildTraderSkills, buildZataraSkills)
import Coal.Snapshot.Types (Info(..), Snapshot(..), opts)
import Coal.Snapshot.UI (display, getHash, getValue, setHTML, setValue, undisplay)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Function (flip, ($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Sequence (fromFoldable)
import Data.String (drop, null)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
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
buildTab "skills" = do
  mi <- getInfo
  case mi of
    Just (Info info) ->
      pure $ fold [ "<div id=\"skillsinfo\">"
                  , "<h1>Class Skills</h1>" 
                  , buildClassSkills info.skills
                  , "<h1>Spookyraven</h1>" 
                  , buildSpookyravenSkills info.skills
                  , "<h1>Gnome</h1>" 
                  , buildGnomeSkills info.skills
                  , "<h1>Daily Dungeon</h1>" 
                  , buildDailyDungeonSkills info.skills
                  , "<h1>Raffle House</h1>" 
                  , buildRaffleSkills info.skills
                  , "<h1>Traveling Trader</h1>" 
                  , buildTraderSkills info.skills
                  , "<h1>Crimbo</h1>" 
                  , buildCrimboSkills info.skills
                  , "<h1>Hobopolis</h1>" 
                  , buildHobopolisSkills info.skills
                  , "<h1>Slime Tube</h1>" 
                  , buildSlimeSkills info.skills
                  , "<h1>Dreadsylvania</h1>" 
                  , buildDreadSkills info.skills
                  , "<h1>PvP Combat</h1>" 
                  , buildPVPSkills info.skills
                  , "<h1>Ascension Rewards</h1>" 
                  , buildAscensionSkills info.skills
                  , "<h1>Mime</h1>" 
                  , buildMimeSkills info.skills
                  , "<h1>Suburbs of Dis</h1>" 
                  , buildDisSkills info.skills
                  , "<h1>Elemental Airport</h1>" 
                  , buildAirportSkills info.skills
                  , "<h1>LT&T</h1>" 
                  , buildLTTSkills info.skills
                  , "<h1>Limited Edition Alpha</h1>" 
                  , buildDeckSkills info.skills
                  , "<h1>Snojo</h1>" 
                  , buildSnojoSkills info.skills
                  , "<h1>Gingerbread City</h1>" 
                  , buildGingerbreadSkills info.skills
                  , "<h1>Spacegate</h1>" 
                  , buildSpacegateSkills info.skills
                  , "<h1>Madame Zatara</h1>" 
                  , buildZataraSkills info.skills
                  , "<h1>Neverending Party</h1>" 
                  , buildPartySkills info.skills
                  , "<h1>Eldritch</h1>" 
                  , buildEldritchSkills info.skills
                  , "<h1>Other</h1>" 
                  , buildMiscSkills info.skills
                  , "</div>"
                  ]
    _ -> pure "No loaded player info."
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
  hash <- getHash
  let initial = drop 1 hash
  if null initial then pure unit else fetch initial
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
