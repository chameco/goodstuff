module Good.Utilities.HTTP where

import Good.Prelude

import Data.Aeson (FromJSON)

import Control.Monad.State (StateT, MonadState, runStateT, get, put)

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Good.Architecture.Input
import Good.Architecture.Inputs.CookieHTTPGet
import Good.Architecture.Inputs.CookieHTTPPost

import Good.Utilities.Scraping

newtype HTTP m a = HTTP { runHTTP :: StateT HTTPState m a }
                        deriving (Functor, Applicative, Monad, MonadIO, MonadState HTTPState, MonadThrow)
newtype HTTPState = HTTPState { cookies :: CookieJar }
                              deriving Show

http :: MonadIO m => HTTP m a -> m a
http k = fst <$> runStateT (runHTTP k) HTTPState { cookies = createCookieJar [] }

httpGetRaw :: MonadIO m => Text -> HTTP m ByteString
httpGetRaw path = do state <- get
                     (resp, cs) <- inputtingState CookieHTTPGetConfig (CookieHTTPGetState $ cookies state) $ do
                         resp <- getRaw $ CookieHTTPGet path
                         (CookieHTTPGetState cs) <- get
                         pure (resp, cs)
                     put HTTPState { cookies = cs }
                     pure resp

httpGetJSON :: (MonadIO m, MonadThrow m, FromJSON a) => Text -> HTTP m a
httpGetJSON path = do state <- get
                      (resp, cs) <- inputtingState CookieHTTPGetConfig (CookieHTTPGetState $ cookies state) $ do
                          resp <- getJSON $ CookieHTTPGet path
                          (CookieHTTPGetState cs) <- get
                          pure (resp, cs)
                      put HTTPState { cookies = cs }
                      pure resp

httpGetHTML :: MonadIO m => Text -> HTTP m HTML
httpGetHTML = fmap toHTML . httpGetRaw

httpPostRaw :: MonadIO m => Text -> [(ByteString, ByteString)] -> HTTP m ByteString
httpPostRaw path params = do state <- get 
                             (resp, cs) <- inputtingState CookieHTTPPostConfig (CookieHTTPPostState $ cookies state) $ do
                                 resp <- getRaw $ CookieHTTPPost path params
                                 (CookieHTTPPostState cs) <- get
                                 pure (resp, cs)
                             put HTTPState { cookies = cs }
                             pure resp

httpPostJSON :: (MonadIO m, MonadThrow m, FromJSON a) => Text -> [(ByteString, ByteString)] -> HTTP m a
httpPostJSON path params = do state <- get 
                              (resp, cs) <- inputtingState CookieHTTPPostConfig (CookieHTTPPostState $ cookies state) $ do
                                  resp <- getJSON $ CookieHTTPPost path params
                                  (CookieHTTPPostState cs) <- get
                                  pure (resp, cs)
                              put HTTPState { cookies = cs }
                              pure resp

httpPostHTML :: MonadIO m => Text -> [(ByteString, ByteString)] -> HTTP m HTML
httpPostHTML path params = toHTML <$> httpPostRaw path params
