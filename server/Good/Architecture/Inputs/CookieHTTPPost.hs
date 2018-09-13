module Good.Architecture.Inputs.CookieHTTPPost where

import Good.Prelude

import Control.Monad.State.Class

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Good.Architecture.Input

data CookieHTTPPost = CookieHTTPPost { cookieHttpPostPath :: Text, cookieHttpPostParams :: [(ByteString, ByteString)]}

instance Input CookieHTTPPost where
    data InputConfig CookieHTTPPost = CookieHTTPPostConfig
    data InputState CookieHTTPPost = CookieHTTPPostState CookieJar
    initialInputState = CookieHTTPPostState $ createCookieJar []
    getRaw l = do man <- liftIO $ newManager tlsManagerSettings
                  initReq <- liftIO $ parseRequest $ toSL $ cookieHttpPostPath l
                  (CookieHTTPPostState cookies) <- get
                  let req = initReq { cookieJar = Just cookies }
                  response <- liftIO $ httpLbs (urlEncodedBody (cookieHttpPostParams l) req) man
                  put . CookieHTTPPostState $ responseCookieJar response
                  pure . toSL $ responseBody response
