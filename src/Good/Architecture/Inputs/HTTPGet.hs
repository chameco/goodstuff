module Good.Architecture.Inputs.HTTPGet where

import Good.Prelude

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Good.Architecture.Input

newtype HTTPGet = HTTPGet { httpGetPath :: Text }

instance Input HTTPGet where
    data InputConfig HTTPGet = HTTPGetConfig
    data InputState HTTPGet = HTTPGetState
    initialInputState = HTTPGetState
    getRaw l = do man <- liftIO $ newManager tlsManagerSettings
                  req <- liftIO $ parseRequest $ toSL $ httpGetPath l
                  response <- liftIO $ httpLbs req man
                  pure . toSL $ responseBody response

--test3 :: MonadIO m => m ()
--test3 = piping HTTPConfig (FSConfig "store2") $ rawpipe id (HTTP "https://chame.co") (FS "bar/baz")

--import Network.HTTP.Types.Header
    --write l d = do man <- liftIO $ newManager tlsManagerSettings
    --               initReq <- liftIO $ parseRequest $ toSL $ httpPath l
    --               let req = initReq { method = "POST"
    --                                 , requestHeaders = [(hContentType, "application/json")]
    --                                 , requestBody = RequestBodyBS d
    --                                 }
    --               liftIO $ void $ httpLbs req man
