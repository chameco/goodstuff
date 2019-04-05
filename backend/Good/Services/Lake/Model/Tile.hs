module Good.Services.Lake.Model.Tile where

import Good.Prelude

import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, genericToEncoding, defaultOptions, allNullaryToStringTag)

import Good.Services.Lake.Interface.Glyph

data Tile = TileVoid
          | TileGround
          | TileGrass
          | TileMud
          | TileSnow
          | TileIce
          | TileWaterMoving
          | TileWaterStill
          | TileTreeBirch
          | TileTreeYew
          | TileTreeOak
          | TileTreeAsh

          | TileHearth
          deriving (Show, Generic)
instance FromJSON Tile where
  parseJSON = genericParseJSON $ defaultOptions { allNullaryToStringTag = False }
instance ToJSON Tile where
  toJSON = genericToJSON $ defaultOptions { allNullaryToStringTag = False }
  toEncoding = genericToEncoding $ defaultOptions { allNullaryToStringTag = False }

tileGlyph :: Tile -> Glyph
tileGlyph TileVoid =
  Glyph [ Frame { frameChar = ' '
                , frameBackupChar = Nothing
                , frameColor = Color () ()
                }
        ]
tileGlyph TileGround =
  Glyph [ Frame { frameChar = '░'
                , frameBackupChar = Just '.'
                , frameColor = Color () ()
                }
        ]
tileGlyph TileGrass =
  Glyph [ Frame { frameChar = '░'
                , frameBackupChar = Just '.'
                , frameColor = Color () ()
                }
        ]
tileGlyph TileMud =
  Glyph [ Frame { frameChar = '≈'
                , frameBackupChar = Just '~'
                , frameColor = Color () ()
                }
        ]
tileGlyph TileSnow =
  Glyph [ Frame { frameChar = '≈'
                , frameBackupChar = Just '~'
                , frameColor = Color () ()
                }
        ]
tileGlyph TileIce =
  Glyph [ Frame { frameChar = '#'
                , frameBackupChar = Nothing
                , frameColor = Color () ()
                }
        ]
tileGlyph TileWaterMoving =
  Glyph [ Frame { frameChar = '~'
                , frameBackupChar = Nothing
                , frameColor = Color () ()
                }
        ]
tileGlyph TileWaterStill =
  Glyph [ Frame { frameChar = '-'
                , frameBackupChar = Nothing
                , frameColor = Color () ()
                }
        ]
tileGlyph TileTreeBirch =
  Glyph [ Frame { frameChar = 'ᛒ'
                , frameBackupChar = Just 'T'
                , frameColor = Color () ()
                }
        ]
tileGlyph TileTreeYew =
  Glyph [ Frame { frameChar = 'ᛇ'
                , frameBackupChar = Just 'Y'
                , frameColor = Color () ()
                }
        ]
tileGlyph TileTreeOak =
  Glyph [ Frame { frameChar = 'ᚪ'
                , frameBackupChar = Just 'K'
                , frameColor = Color () ()
                }
        ]
tileGlyph TileTreeAsh =
  Glyph [ Frame { frameChar = 'ᚫ'
                , frameBackupChar = Just 'H'
                , frameColor = Color () ()
                }
        ]
tileGlyph TileHearth =
  Glyph [ Frame { frameChar = 'ᛟ'
                , frameBackupChar = Just '&'
                , frameColor = Color () ()
                }
        ]
