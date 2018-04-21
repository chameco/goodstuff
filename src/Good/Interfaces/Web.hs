module Good.Interfaces.Web (
  module Network.HTTP.Types.Status,
  WebError (..),
  Serving, serving,
  Handling, handling,
  Request (..),
  Response (..),
  middleware,
  params, param,
  ) where

import Good.Prelude

import Data.Aeson (ToJSON)

import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html)

import Network.HTTP.Types.Status

import qualified Network.Wai as Wai
import qualified Web.Scotty.Trans as Scotty
import qualified Web.Scotty.Internal.Types as Scotty.Types

data WebError = WebError Status Text deriving Show
instance Exception WebError
instance Scotty.Types.ScottyError WebError where
    stringError = WebError internalServerError500 . toSL
    showError (WebError _ t) = toSL t

newtype Serving m a = Serving { runServing :: Scotty.ScottyT WebError m a }
                              deriving (Functor, Applicative, Monad)

serving :: Int -> Serving IO () -> IO ()
serving port = Scotty.scottyT port liftIO . runServing

middleware :: Wai.Middleware -> Serving m ()
middleware = Serving . Scotty.middleware

deriving instance MonadThrow m => MonadThrow (Scotty.ActionT WebError m)
deriving instance MonadCatch m => MonadCatch (Scotty.ActionT WebError m)
newtype Handling m a = Handling { runHandling :: Scotty.ActionT WebError m a }
                                deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

data Request = Get Text
             | Post Text
             deriving (Show, Eq)

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

handling :: (MonadIO m, MonadCatch m) => Request -> Handling m Response -> Serving m ()
handling (Get route) = Serving . Scotty.get (Scotty.Types.Literal $ toSL route) . runHandling . respond
handling (Post route) = Serving . Scotty.post (Scotty.Types.Literal $ toSL route) . runHandling . respond

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
