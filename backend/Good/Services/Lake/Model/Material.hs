module Good.Services.Lake.Model.Material where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Services.Lake.Interface.Glyph

data Class = ClassHuman
           | ClassAnimal 
           | ClassStrange
           | ClassInanimate
           deriving (Show, Generic)
instance FromJSON Class where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Class where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

data Material = MaterialNative -- humans
              | MaterialForeigner
              | MaterialPrimitive

              | MaterialLizard -- animals
              | MaterialBird
              | MaterialBear
              | MaterialBoar
              | MaterialSeaCreature
              | MaterialDog
              | MaterialCat
              | MaterialHorse
              | MaterialGame

              | MaterialDeity -- strange creatures
              | MaterialGiant
              | MaterialCorporealTroll
              | MaterialIncorporealTroll
              | MaterialKin

              | MaterialWood -- inanimate parts
              | MaterialStone
              | MaterialIron
              | MaterialBronze
              | MaterialSilver
              deriving (Show, Generic)
instance FromJSON Material where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Material where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

materialClass :: Material -> Class
materialClass MaterialNative = ClassHuman
materialClass MaterialForeigner = ClassHuman
materialClass MaterialPrimitive = ClassHuman

materialClass MaterialLizard = ClassAnimal
materialClass MaterialBird = ClassAnimal
materialClass MaterialBear = ClassAnimal
materialClass MaterialBoar = ClassAnimal
materialClass MaterialSeaCreature = ClassAnimal
materialClass MaterialDog = ClassAnimal
materialClass MaterialCat = ClassAnimal
materialClass MaterialHorse = ClassAnimal
materialClass MaterialGame = ClassAnimal

materialClass MaterialDeity = ClassStrange
materialClass MaterialGiant = ClassStrange
materialClass MaterialCorporealTroll = ClassStrange
materialClass MaterialIncorporealTroll = ClassStrange
materialClass MaterialKin = ClassStrange

materialClass MaterialWood = ClassInanimate
materialClass MaterialStone = ClassInanimate
materialClass MaterialIron = ClassInanimate
materialClass MaterialBronze = ClassInanimate
materialClass MaterialSilver = ClassInanimate

classGlyph :: Class -> Glyph
classGlyph ClassHuman =
  Glyph [ Frame { frameChar = '@'
                , frameBackupChar = Nothing
                , frameColor = Color () ()
                }
        ]
classGlyph ClassAnimal =
  Glyph [ Frame { frameChar = 'H'
                , frameBackupChar = Nothing
                , frameColor = Color () ()
                }
        ]
classGlyph ClassStrange =
  Glyph [ Frame { frameChar = '&'
                , frameBackupChar = Nothing
                , frameColor = Color () ()
                }
        ]
classGlyph ClassInanimate =
  Glyph [ Frame { frameChar = 'R'
                , frameBackupChar = Nothing
                , frameColor = Color () ()
                }
        ]

materialAdjective :: Material -> Text
materialAdjective MaterialNative = "native"
materialAdjective MaterialForeigner = "foreign"
materialAdjective MaterialPrimitive = "primitive"

materialAdjective MaterialLizard = "reptilian"
materialAdjective MaterialBird = "avian"
materialAdjective MaterialBear = "ursine"
materialAdjective MaterialBoar = "porcine"
materialAdjective MaterialSeaCreature = "piscine"
materialAdjective MaterialDog = "canine"
materialAdjective MaterialCat = "feline"
materialAdjective MaterialHorse = "equine"
materialAdjective MaterialGame = "scrawny"

materialAdjective MaterialDeity = "arcane"
materialAdjective MaterialGiant = "jötunn"
materialAdjective MaterialCorporealTroll = "supernatural"
materialAdjective MaterialIncorporealTroll = "incomprehensible"
materialAdjective MaterialKin = "kindred"

materialAdjective MaterialWood = "wooden"
materialAdjective MaterialStone = "stone"
materialAdjective MaterialIron = "iron"
materialAdjective MaterialBronze = "bronze"
materialAdjective MaterialSilver = "silver"
