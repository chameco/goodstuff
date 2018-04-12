module Good.Interfaces.Web where

import Good.Prelude

import Data.Aeson (ToJSON)

import Network.HTTP.Types.Status

import qualified Network.Wai as Wai
import qualified Web.Scotty.Trans as Scotty
import qualified Web.Scotty.Internal.Types as Scotty.Types

data WebError = WebError Status Text deriving Show
instance Exception WebError

instance Scotty.Types.ScottyError WebError where
    stringError = WebError internalServerError500 . toSL
    showError (WebError _ t) = toSL t

deriving instance MonadThrow m => MonadThrow (Scotty.ActionT WebError m)

newtype Serving m a = Serving { runServing :: Scotty.ScottyT WebError m a }
                              deriving (Functor, Applicative, Monad)

serving :: Int -> Serving IO () -> IO ()
serving port = Scotty.scottyT port liftIO . runServing

middleware :: Wai.Middleware -> Serving m ()
middleware = Serving . Scotty.middleware

newtype Handling m a = Handling { runHandling :: Scotty.ActionT WebError m a }
                                deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

data Request = Get Text
             | Post Text
             deriving (Show, Eq)

data Response where
    Plaintext :: Text -> Response
    --Markup :: HTML -> Response (use blaze-html or some othr rendering engine here)
    JSON :: ToJSON a => a -> Response
    Raw :: ByteString -> Response

respond :: MonadThrow m => Handling m Response -> Handling m ()
respond body = do resp <- body
                  case resp of (Plaintext x) -> Handling . Scotty.text $ toSL x
                               (JSON x) -> Handling $ Scotty.json x
                               (Raw x) -> Handling . Scotty.raw $ toSL x

handling :: (MonadIO m, MonadThrow m) => Request -> Handling m Response -> Serving m ()
handling (Get route) = Serving . Scotty.get (Scotty.Types.Literal $ toSL route) . runHandling . respond
handling (Post route) = Serving . Scotty.post (Scotty.Types.Literal $ toSL route) . runHandling . respond

params :: Monad m => Handling m [(Text, Text)]
params = Handling . fmap convert $ Scotty.params
    where convert :: [Scotty.Types.Param] -> [(Text, Text)]
          convert [] = []
          convert ((x, y):xs) = (toSL x, toSL y):convert xs

param :: MonadThrow m => Text -> Handling m Text
param x = params >>= f x
    where f :: MonadThrow m => Text -> [(Text, Text)] -> Handling m Text
          f _ [] = throwM $ WebError badRequest400 "Parameter not found"
          f y ((k, v):xs) | y == k = pure v | otherwise = f y xs
