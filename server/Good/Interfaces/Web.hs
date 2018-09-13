module Good.Interfaces.Web (
  module Network.HTTP.Types.Status,
  WebError (..),
  Serving (..), serving,
  Handling (..), handling,
  Request (..),
  Response (..),
  File (..),
  middleware,
  params, param, files
  ) where

import Good.Prelude

import Data.Aeson (ToJSON)

import Control.Monad.State

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html)

import Network.HTTP.Types.Status

import qualified Network.Wai as Wai
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Parse (fileName, fileContentType, fileContent)

import qualified Network.WebSockets as WS

import qualified Web.Scotty.Trans as Scotty
import qualified Web.Scotty.Internal.Types as Scotty.Types

data WebError = WebError Status Text deriving Show
instance Exception WebError
instance Scotty.Types.ScottyError WebError where
  stringError = WebError internalServerError500 . toSL
  showError (WebError _ t) = toSL t

newtype Serving m a = Serving { runServing :: Scotty.ScottyT WebError m a }
                              deriving newtype (Functor, Applicative, Monad)

serving :: Int -> Serving IO () -> IO ()
serving port = Scotty.scottyT port liftIO . runServing

middleware :: Wai.Middleware -> Serving m ()
middleware = Serving . Scotty.middleware

newtype Handling m a = Handling { runHandling :: Scotty.ActionT WebError m a }
                                deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

data RequestType = G | P | S
data Request (r :: RequestType) where
  Get :: Text -> Request 'G
  Post :: Text -> Request 'P
  Socket :: Text -> Request 'S
type family RequestHandler (h :: RequestType) (m :: (* -> *)) (a :: *) :: *
type instance RequestHandler 'G m a = Handling m a
type instance RequestHandler 'P m a = Handling m a
type instance RequestHandler 'S _ _ = WS.Connection -> IO ()

data Response where
  Plaintext :: Text -> Response
  Markup :: Html -> Response
  JSON :: ToJSON a => a -> Response
  Raw :: ByteString -> Response
  Redirect :: Text -> Response

respond :: (MonadIO m, MonadCatch m) => Handling m Response -> Handling m ()
respond body = catch (do resp <- body; case resp of (Plaintext x) -> Handling . Scotty.text $ toSL x
                                                    (Markup x) -> Handling . Scotty.html . toSL $ renderHtml x
                                                    (JSON x) -> Handling $ Scotty.json x
                                                    (Raw x) -> Handling . Scotty.raw $ toSL x
                                                    (Redirect x) -> Handling . Scotty.redirect $ toSL x)
                     (\(WebError s t) -> Handling (Scotty.text (toSL t) >> Scotty.status s))

handling :: (MonadIO m, MonadCatch m) => Request t -> RequestHandler t m Response -> Serving m ()
handling (Get route) h = Serving . Scotty.get (Scotty.Types.Capture $ toSL route) . runHandling $ respond h
handling (Post route) h = Serving . Scotty.post (Scotty.Types.Capture $ toSL route) . runHandling $ respond h
handling (Socket route) h = middleware (websocketsOr WS.defaultConnectionOptions handler)
  where handler :: WS.PendingConnection -> IO ()
        handler pending = if WS.requestPath (WS.pendingRequest pending) == toSL route
                          then WS.acceptRequest pending >>= h
                          else WS.rejectRequest pending ""

params :: Monad m => Handling m [(Text, Text)]
params = Handling . fmap convert $ Scotty.params
  where convert :: [Scotty.Types.Param] -> [(Text, Text)]
        convert [] = []
        convert ((x, y):xs) = (toSL x, toSL y):convert xs

param :: MonadThrow m => Text -> Handling m Text
param x = params >>= f
  where f :: MonadThrow m => [(Text, Text)] -> Handling m Text
        f [] = throwM . WebError badRequest400 $ mconcat ["Required parameter \"", x, "\" not found"]
        f ((k, v):xs) | x == k = pure v | otherwise = f xs

data File = File Text Text Text ByteString deriving (Show, Eq)

files :: Monad m => Handling m [File]
files = Handling . fmap convert $ Scotty.files
  where convert :: [Scotty.Types.File] -> [File]
        convert [] = []
        convert ((x, f):xs) = File (toSL x) (toSL $ fileName f) (toSL $ fileContentType f) (toSL $ fileContent f):convert xs
