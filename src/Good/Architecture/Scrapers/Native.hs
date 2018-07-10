module Good.Architecture.Scrapers.Native where

import Good.Prelude

import Control.Monad.State (StateT, MonadState, runStateT, get, put)

import Network.HTTP.Client (CookieJar, createCookieJar)

import Good.Architecture.Scraper
import qualified Good.Architecture.Input as In
import Good.Architecture.Inputs.CookieHTTPGet
import Good.Architecture.Inputs.CookieHTTPPost


data Native = Native
newtype NativeMonadTrans m a = NativeMonadTrans { runHTTP :: StateT NativeState m a }
                                                deriving (Functor, Applicative, Monad, MonadIO, MonadState NativeState, MonadThrow, MonadCatch)
newtype NativeState = NativeState { cookies :: CookieJar }
                                  deriving Show

instance Scraper Native where
    type Scraping Native = NativeMonadTrans
    scraping k = fst <$> runStateT (runHTTP k) NativeState { cookies = createCookieJar [] }
    getRaw path = do state <- get
                     (resp, cs) <- In.inputtingState CookieHTTPGetConfig (CookieHTTPGetState $ cookies state) $ do
                         resp <- In.getRaw $ CookieHTTPGet path
                         (CookieHTTPGetState cs) <- get
                         pure (resp, cs)
                     put NativeState { cookies = cs }
                     pure resp
    getJSON path = do state <- get
                      (resp, cs) <- In.inputtingState CookieHTTPGetConfig (CookieHTTPGetState $ cookies state) $ do
                          resp <- In.getJSON $ CookieHTTPGet path
                          (CookieHTTPGetState cs) <- get
                          pure (resp, cs)
                      put NativeState { cookies = cs }
                      pure resp
    postRaw path params = do state <- get 
                             (resp, cs) <- In.inputtingState CookieHTTPPostConfig (CookieHTTPPostState $ cookies state) $ do
                                 resp <- In.getRaw $ CookieHTTPPost path params
                                 (CookieHTTPPostState cs) <- get
                                 pure (resp, cs)
                             put NativeState { cookies = cs }
                             pure resp
    postJSON path params = do state <- get 
                              (resp, cs) <- In.inputtingState CookieHTTPPostConfig (CookieHTTPPostState $ cookies state) $ do
                                  resp <- In.getJSON $ CookieHTTPPost path params
                                  (CookieHTTPPostState cs) <- get
                                  pure (resp, cs)
                              put NativeState { cookies = cs }
                              pure resp
