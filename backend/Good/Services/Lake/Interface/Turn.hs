module Good.Services.Lake.Interface.Turn where

import Good.Prelude

import Good.Services.Lake.Utility.Coords
import Good.Services.Lake.Model.Universe

data Action = ActionMove Direction
            | ActionPickup Text
            | ActionDrop Text
            deriving Show

data Turn = Turn { turnAction :: Action
                 , turnPlayer :: Text
                 }
  deriving Show

processTurns :: (MonadIO m, MonadCatch m, MonadState Universe m) => [Turn] -> m ()
processTurns _ = pure ()
