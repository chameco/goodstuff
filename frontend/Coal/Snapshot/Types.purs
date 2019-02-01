module Coal.Snapshot.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Show (class Show)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)

opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

data Snapshot = Snapshot
  { snapshotTime :: String
  , iotm :: Array Boolean
  , scSkills :: Array Boolean
  , ttSkills :: Array Boolean
  , pmSkills :: Array Boolean
  , saSkills :: Array Boolean
  , dbSkills :: Array Boolean
  , atSkills :: Array Boolean
  }
derive instance genericSnapshot :: Generic Snapshot _
instance showSnapshot :: Show Snapshot where show = genericShow
instance decodeSnapshot :: Decode Snapshot where decode = genericDecode opts
instance encodeSnapshot :: Encode Snapshot where encode = genericEncode opts

data Info = Info
  { infoTime :: String
  , name :: String
  , title :: String
  , avatar :: String
  }
derive instance genericInfo :: Generic Info _
instance showInfo :: Show Info where show = genericShow
instance decodeInfo :: Decode Info where decode = genericDecode opts
instance encodeInfo :: Encode Info where encode = genericEncode opts
