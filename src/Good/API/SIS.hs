module Good.API.SIS where

import Good.Prelude

import Network.Wai.Middleware.Cors (simpleCors)

import Good.Utilities.Web
import Good.Utilities.Scraping
import Good.Services.SIS

sis :: Serving IO ()
sis = do
    middleware simpleCors
    handling (Post "/sis/name") $ do
        rin <- param "rin"
        pass <- param "pass"
        name <- scraping $ login rin pass
        pure (Plaintext name)
    handling (Get "/sis/foo") $ pure (Plaintext "bar")
