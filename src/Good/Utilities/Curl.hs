module Good.Utilities.Curl where

import Good.Prelude

import Data.Aeson (FromJSON)

import System.IO.Temp (withSystemTempDirectory)

import Good.Architecture.Input

import Good.Utilities.Process
import Good.Utilities.Scraping

newtype Curl m a = Curl { runCurl :: ReaderT CurlState m a }
                        deriving (Functor, Applicative, Monad, MonadIO, MonadReader CurlState, MonadThrow)
newtype CurlState = CurlState { cookieFile :: FilePath }
                              deriving Show

curl :: (MonadIO m, MonadMask m) => Curl m a -> m a
curl k = withSystemTempDirectory "goodstuff" $ \tmp ->
             runReaderT (runCurl k) CurlState { cookieFile = tmp </> "cookiejar.txt"}

curlGetRaw :: MonadIO m => Text -> Curl m ByteString
curlGetRaw path = do (CurlState jar) <- ask
                     inputting ProcessInputConfig . getRaw $ Process "curl" ["-L", "-c", toSL jar, "-b", toSL jar, path]

curlGetJSON :: (MonadIO m, MonadThrow m) => Text -> Curl m ByteString
curlGetJSON path = do (CurlState jar) <- ask
                      inputting ProcessInputConfig . getRaw $ Process "curl" ["-L", "-c", toSL jar, "-b", toSL jar, path]

curlGetHTML :: MonadIO m => Text -> Curl m HTML
curlGetHTML = fmap toHTML . curlGetRaw

curlPostRaw :: MonadIO m => Text -> [(ByteString, ByteString)] -> Curl m ByteString
curlPostRaw path params = do (CurlState jar) <- ask
                             inputting ProcessInputConfig . getRaw $ Process "curl" (convert params <> ["-L", "-c", toSL jar, "-b", toSL jar, path])
    where convert :: [(ByteString, ByteString)] -> [Text]
          convert [] = []
          convert ((k, v):xs) = "-F":mconcat [toSL k, "=", toSL v]:convert xs

curlPostJSON :: (MonadIO m, MonadThrow m, FromJSON a) => Text -> [(ByteString, ByteString)] -> Curl m a
curlPostJSON path params = do (CurlState jar) <- ask
                              inputting ProcessInputConfig . getJSON $ Process "curl" (convert params <> ["-L", "-c", toSL jar, "-b", toSL jar, path])
    where convert :: [(ByteString, ByteString)] -> [Text]
          convert [] = []
          convert ((k, v):xs) = "-F":mconcat [toSL k, "=", toSL v]:convert xs

curlPostHTML :: MonadIO m => Text -> [(ByteString, ByteString)] -> Curl m HTML
curlPostHTML path params = toHTML <$> curlPostRaw path params
