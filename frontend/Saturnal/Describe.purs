module Saturnal.Describe where
import Saturnal.Types

import Control.Bind (bind, discard, pure)
import Data.Array (fold, intercalate)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Saturnal.Event (listen)
import Saturnal.State (State(..), getState, setState)
import Saturnal.UI (description)

describeCell :: Cell -> String
describeCell (Cell c) = fold [ intercalate ", " $ describeTag <$> c.cellTags, "<hr>"
                             , "<h4>Entities: </h4>", intercalate ", " $ describeEntityInline <$> c.cellEntities, "<hr>"
                             , "<h4>Structures: </h4>", intercalate ", " $ describeStructureInline <$> c.cellStructures
                             ]

listenDescribeCell :: Cell -> Effect Unit
listenDescribeCell (Cell c) = do
  traverse_ listenDescribeEntity c.cellEntities
  traverse_ listenDescribeStructure c.cellStructures

describeTag :: Tag -> String
describeTag (Tag t) = t
describeTag (TagData t d) = t <> ": " <> describeTagData d

describeTagData :: TagData -> String
describeTagData (TagDataInt i) = show i
describeTagData (TagDataPoint x y) = fold ["(", show x, ", ", show y, ")"]

describeAction :: String -> ActionDescription -> String
describeAction uuid (ActionDescription a) = fold ["<button id=\"describe-", uuid, "-", a.actionName, "\">", a.actionDisplay, "</button>"]

describeEntityInline :: Entity -> String
describeEntityInline (Entity e) = fold [ "<span id=\"describe-", e.entityID, "\">"
                                       , "Rank ", show e.entityRank, " (", e.entityOwner, ")"
                                       , "</span>"
                                       ]
describeEntity :: Entity -> String
describeEntity (Entity e) = fold [ "Rank ", show e.entityRank, " (", e.entityOwner, ")", "<hr>"
                                 , intercalate ", " $ describeTag <$> e.entityTags, "<hr>"
                                 , "<h4>Actions: </h4>", fold $ describeAction e.entityID <$> e.entityActions 
                                 ]

listenDescribeEntity :: Entity -> Effect Unit
listenDescribeEntity (Entity e) = listen ("describe-" <> e.entityID) "click" $ do
  description ("describe-" <> e.entityID) "Entity" $ describeEntity (Entity e)
  for_ e.entityActions $ \(ActionDescription a) ->
    listen (fold ["describe-", e.entityID, "-", a.actionName]) "click" $ do
      state <- getState
      case state of
        Just (State v m b) ->
          case v.secondary of
            Just (Tuple x y) ->
              let move = MoveEntity { moveEntityID: e.entityID
                                    , moveEntityAction: a.actionName
                                    , moveEntityX: x
                                    , moveEntityY: y }
              in setState $ State v (m <> [move]) b
            _ -> pure unit
        Nothing -> pure unit

describeStructureInline :: Structure -> String
describeStructureInline (Structure s) = fold [ "<span id=\"describe-", s.structureID, "\">"
                                             , s.structureOwner
                                             , "</s>"
                                             ]

describeStructure :: Structure -> String
describeStructure (Structure s) = fold [ "Owned by ", s.structureOwner, "<hr>"
                                       , intercalate ", " $ describeTag <$> s.structureTags
                                       ]

listenDescribeStructure :: Structure -> Effect Unit
listenDescribeStructure (Structure s) = listen ("describe-" <> s.structureID) "click" $ do
  description ("describe-" <> s.structureID) "Structure" $ describeStructure (Structure s)
