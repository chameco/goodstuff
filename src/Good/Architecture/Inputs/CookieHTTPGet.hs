module Good.Architecture.Inputs.CookieHTTPGet where

import Good.Prelude

import Control.Monad.State.Class

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Good.Architecture.Input

newtype CookieHTTPGet = CookieHTTPGet { cookieHttpGetPath :: Text }

instance Input CookieHTTPGet where
    data InputConfig CookieHTTPGet = CookieHTTPGetConfig
    data InputState CookieHTTPGet = CookieHTTPGetState CookieJar
    initialInputState = CookieHTTPGetState $ createCookieJar []
    getRaw l = do man <- liftIO $ newManager tlsManagerSettings
                  initreq <- liftIO . parseRequest . toSL $ cookieHttpGetPath l
                  (CookieHTTPGetState cookies) <- get
                  let req = initreq { cookieJar = Just cookies }
                  response <- liftIO $ httpLbs req man
                  put . CookieHTTPGetState $ responseCookieJar response
                  pure . toSL $ responseBody response
