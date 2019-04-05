module Good.Services.Sim.Describe where

import Good.Prelude

import Data.Char (isUpper)
import qualified Data.Text as Text

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Good.Services.Sim.Item
import Good.Services.Sim.Entity
import Good.Services.Sim.Feature
import Good.Services.Sim.Gateway
import Good.Services.Sim.Room
import Good.Services.Sim.Area
import Good.Services.Sim.Universe

class Describable a where
  describe :: Universe -> a -> Description
  buildLink :: a -> (Text, Text)

data Description = Description
  { title :: Text
  , body :: H.Html
  }

an :: Text -> Text
an t | isUpper $ Text.head t = ""
     | Text.head t `elem` ("aeiouy" :: String) = "an"
     | otherwise = "a"

renderLink :: (Text, Text) -> H.Html
renderLink (url, text) = H.a ! A.href (fromString $ unpack url) $ H.toHtml text

instance Describable Item where
  buildLink i = (an $ itemName i, "/sim/describe-item/" <> itemId i)
  describe _ i = Description
    { title = an $ itemName i
    , body = H.p . H.toHtml $ itemDescription i
    }

instance Describable Entity where
  buildLink e = (an $ entityName e, "/sim/describe-entity/" <> entityId e)
  describe u e = Description
    { title = an $ entityName e
    , body = mconcat [ H.p . H.toHtml $ entityDescription e
                     , "Holding:"
                     , H.ul $ mconcat (H.li . renderLink . buildLink <$> catMaybes (lookupItem u <$> entityInventory e))
                     ]
    }

instance Describable Feature where
  buildLink f = (an $ featureName f, "/sim/describe-feature/" <> featureId f)
  describe _ f = Description
    { title = an $ featureName f
    , body = H.p . H.toHtml $ featureDescription f
    }

instance Describable Gateway where
  buildLink g = (an $ gatewayName g, "/sim/describe-gateway/" <> gatewayId g)
  describe u g = Description
    { title = an $ gatewayName g
    , body = mconcat [ H.p . H.toHtml $ gatewayDescription g
                     , "Leads to ", case lookupRoom u $ gatewayTarget g of
                         Just r -> renderLink $ buildLink r
                         Nothing -> "nowhere"
                       ]
    }

instance Describable Room where
  buildLink r = (an $ roomName r, "/sim/describe-room/" <> roomId r)
  describe u r = Description
    { title = an $ roomName r
    , body = mconcat [ H.p . H.toHtml $ roomDescription r
                     , "Notable features:"
                     , H.ul $ mconcat (H.li . renderLink . buildLink <$> catMaybes (lookupItem u <$> roomFeatures r))
                     , "Connections:"
                     , H.ul $ mconcat (H.li . renderLink . buildLink <$> catMaybes (lookupItem u <$> roomGateways r))
                     , "Inhabitants:"
                     , H.ul $ mconcat (H.li . renderLink . buildLink <$> catMaybes (lookupItem u <$> roomEntities r))
                     ]
    }

instance Describable Area where
  buildLink a = (an $ areaName a, "/sim/describe-area/" <> areaId a)
  describe u a = Description
    { title = an $ areaName a
    , body = mconcat [ H.p . H.toHtml $ areaDescription a
                     , "Within:"
                     , H.ul $ mconcat (H.li . renderLink . buildLink <$> catMaybes (lookupItem u <$> areaRooms a))
                     ]
    }
