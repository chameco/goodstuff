module Good.Architecture.Inputs.HTTPPost where

import Good.Prelude

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Good.Architecture.Input

data HTTPPost = HTTPPost { httpPostPath :: Text, httpPostParams :: [(ByteString, ByteString)]}

instance Input HTTPPost where
    data InputConfig HTTPPost = HTTPPostConfig
    data InputState HTTPPost = HTTPPostState
    initialInputState = HTTPPostState
    getRaw l = do man <- liftIO $ newManager tlsManagerSettings
                  initReq <- liftIO $ parseRequest $ toSL $ httpPostPath l
                  response <- liftIO $ httpLbs (urlEncodedBody (httpPostParams l) initReq) man
                  pure . toSL $ responseBody response
