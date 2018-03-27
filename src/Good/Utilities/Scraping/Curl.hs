module Good.Utilities.Scraping.Curl where

import Good.Prelude

import System.IO.Temp (emptySystemTempFile)

import qualified Good.Architecture.Input as In

import Good.Utilities.Process
import Good.Utilities.Scraping

data Curl = Curl
newtype CurlMonadTrans m a = CurlMonadTrans { runCurl :: ReaderT CurlState m a }
                                            deriving (Functor, Applicative, Monad, MonadIO, MonadReader CurlState, MonadThrow)
newtype CurlState = CurlState { cookieFile :: FilePath }
                              deriving Show

convert :: [(ByteString, ByteString)] -> Text
convert [] = mempty
convert [(k, v)] = mconcat [toSL k, "=", toSL v]
convert ((k, v):x:xs) = mconcat [toSL k, "=", toSL v, "&", convert (x:xs)]

instance Scraper Curl where
    type Scraping Curl = CurlMonadTrans
    scraping k = do jar <- liftIO $ emptySystemTempFile "goodstuff-curl-cookiejar"
                    runReaderT (runCurl k) CurlState { cookieFile = jar}
    getRaw path = do (CurlState jar) <- ask
                     In.inputting ProcessInputConfig . In.getRaw $ Process "curl" ["-L", "-c", toSL jar, "-b", toSL jar, path]
    getJSON path = do (CurlState jar) <- ask
                      In.inputting ProcessInputConfig . In.getJSON $ Process "curl" ["-L", "-c", toSL jar, "-b", toSL jar, path]
    postRaw path params = do (CurlState jar) <- ask
                             In.inputting ProcessInputConfig . In.getRaw $ Process "curl" ["-L", "-c", toSL jar, "-b", toSL jar, path, "-d", convert params]
    postJSON path params = do (CurlState jar) <- ask
                              In.inputting ProcessInputConfig . In.getJSON $ Process "curl" ["-L", "-c", toSL jar, "-b", toSL jar, path, "-d", convert params]
