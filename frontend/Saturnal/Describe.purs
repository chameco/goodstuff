module Saturnal.Describe where
import Saturnal.Types

import Control.Bind (discard)

import Data.Array (fold, intercalate)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (traverse_)
import Data.Unit (Unit)
import Effect (Effect)
import Saturnal.Event (listen)
import Saturnal.UI (description)

describeCell :: Cell -> String
describeCell (Cell c) = fold [ intercalate ", " $ describeTag <$> c.cellTags, "<hr>"
                             , "<h4>Entities: </h4>", intercalate ", " $ describeEntityInline <$> c.cellEntities, "<hr>"
                             , "<h4>Structures: </h4>", intercalate ", " $ describeStructureInline <$> c.cellStructures, "<hr>"
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

describeEntityInline :: Entity -> String
describeEntityInline (Entity e) = fold [ "<span id=\"describe-", e.entityID, "\">"
                                       , "Rank ", show e.entityRank, " (", e.entityOwner, ")"
                                       , "</span>"
                                       ]
describeEntity :: Entity -> String
describeEntity (Entity e) = fold [ "Rank ", show e.entityRank, " (", e.entityOwner, ")", "<hr>"
                                 , intercalate ", " $ describeTag <$> e.entityTags
                                 ]

listenDescribeEntity :: Entity -> Effect Unit
listenDescribeEntity (Entity e) = listen ("describe-" <> e.entityID) "click" $ do
  description ("describe-" <> e.entityID) "Entity" $ describeEntity (Entity e)

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
