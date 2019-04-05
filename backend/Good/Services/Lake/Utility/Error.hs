module Good.Services.Lake.Utility.Error where

import Good.Prelude

newtype LakeError = LakeError Text deriving Show
instance Exception LakeError
