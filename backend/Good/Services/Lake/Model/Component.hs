module Good.Services.Lake.Model.Component where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Services.Lake.Interface.Glyph
import Good.Services.Lake.Model.Material

data ComponentType = ComponentSupertorso -- level 0

                   | ComponentTorso -- level 1
                   | ComponentPole
                   
                   | ComponentHead -- level 2
                   | ComponentArm
                   | ComponentLeg
                   | ComponentWings
                   | ComponentTreads
                   | ComponentHandle
                   | ComponentStock

                   | ComponentHand -- level 3
                   | ComponentFoot
                   | ComponentClaw
                   | ComponentEye
                   | ComponentEar
                   | ComponentNose
                   | ComponentJaw
                   | ComponentTongue
                   | ComponentWhip
                   | ComponentSpike
                   | ComponentBlade
                   | ComponentBarrel
                   | ComponentAction
                   deriving (Show, Generic)
instance FromJSON ComponentType where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON ComponentType where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

data ComponentDamage = ComponentDamageNone
                     | ComponentDamageMinor
                     | ComponentDamageMajor
                     | ComponentDamageSevere
                     | ComponentDamageTotal
                     deriving (Show, Generic)
instance FromJSON ComponentDamage where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON ComponentDamage where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

data ComponentProperty = ComponentPropertyGlowing
                       deriving (Show, Generic)
instance FromJSON ComponentProperty where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON ComponentProperty where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

data Component = Component { componentType :: ComponentType
                           , componentMaterial :: Material
                           , componentDamage :: ComponentDamage
                           , componentProperties :: [ComponentProperty]
                           , componentChildren :: [Component]
                           }
  deriving (Show, Generic)
instance FromJSON Component where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Component where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

componentTypeLevel :: ComponentType -> Int
componentTypeLevel ComponentSupertorso = 0

componentTypeLevel ComponentTorso = 1
componentTypeLevel ComponentPole = 1

componentTypeLevel ComponentHead = 2
componentTypeLevel ComponentArm = 2
componentTypeLevel ComponentLeg = 2
componentTypeLevel ComponentWings = 2
componentTypeLevel ComponentTreads = 2
componentTypeLevel ComponentHandle = 2
componentTypeLevel ComponentStock = 2

componentTypeLevel ComponentHand = 3
componentTypeLevel ComponentFoot = 3
componentTypeLevel ComponentClaw = 3
componentTypeLevel ComponentEye = 3
componentTypeLevel ComponentEar = 3
componentTypeLevel ComponentNose = 3
componentTypeLevel ComponentJaw = 3
componentTypeLevel ComponentTongue = 3
componentTypeLevel ComponentWhip = 3
componentTypeLevel ComponentSpike = 3
componentTypeLevel ComponentBlade = 3
componentTypeLevel ComponentBarrel = 3
componentTypeLevel ComponentAction = 3

componentTypeNoun :: ComponentType -> Text
componentTypeNoun ComponentSupertorso = "body"

componentTypeNoun ComponentTorso = "torso"
componentTypeNoun ComponentPole = "pole"

componentTypeNoun ComponentHead = "head"
componentTypeNoun ComponentArm = "arm"
componentTypeNoun ComponentLeg = "leg"
componentTypeNoun ComponentWings = "wings"
componentTypeNoun ComponentTreads = "treads"
componentTypeNoun ComponentHandle = "handle"
componentTypeNoun ComponentStock = "stock"

componentTypeNoun ComponentHand = "hand"
componentTypeNoun ComponentFoot = "foot"
componentTypeNoun ComponentClaw = "claw"
componentTypeNoun ComponentEye = "eye"
componentTypeNoun ComponentEar = "ear"
componentTypeNoun ComponentNose = "nose"
componentTypeNoun ComponentJaw = "jaw"
componentTypeNoun ComponentTongue = "tongue"
componentTypeNoun ComponentWhip = "tentacle"
componentTypeNoun ComponentSpike = "horn"
componentTypeNoun ComponentBlade = "blade"
componentTypeNoun ComponentBarrel = "barrel"
componentTypeNoun ComponentAction = "action"

componentDamageAdjective :: ComponentDamage -> Text
componentDamageAdjective ComponentDamageNone = "undamaged"
componentDamageAdjective ComponentDamageMinor = "minorly damaged"
componentDamageAdjective ComponentDamageMajor = "majorly damaged"
componentDamageAdjective ComponentDamageSevere = "severely damaged"
componentDamageAdjective ComponentDamageTotal = "completely destroyed"

componentDamageColor :: ComponentDamage -> Color
componentDamageColor _ = Color () ()
