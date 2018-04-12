{-# OPTIONS_GHC -fno-warn-orphans #-}

module Good.Architecture.Parsing where

import Good.Prelude

import qualified Control.Monad.Catch

import qualified Text.Parsec as Parsec

newtype ParseError = ParseError ByteString deriving (Show, Read)
instance Exception ParseError

instance MonadThrow (Parsec.ParsecT ByteString () m) where
    throwM e = fail (displayException e)

newtype Parsing m a = Parsing { runParsing :: Parsec.ParsecT ByteString () m a }
                              deriving (Functor, Applicative, Monad, Alternative, MonadTrans, MonadThrow)

parsing :: MonadThrow m => Text -> ByteString -> Parsing m a -> m a
parsing s d p = do x <- Parsec.runParserT (runParsing p) () (toSL s) d
                   case x of Left err -> throwM . ParseError . toSL $ show err
                             Right y -> pure y

readTrust :: (Monad m, Read a) => String -> Parsing m a
readTrust x = case readMay x of Just y -> pure y
                                Nothing -> throwM . ParseError . toSL $ "Failed to read" <> x

char :: Monad m => Char -> Parsing m Char
char = Parsing . Parsec.char

whitespace :: Monad m => Parsing m ()
whitespace = Parsing Parsec.spaces

many1 :: Monad m => Parsing m a -> Parsing m [a]
many1 = Parsing . Parsec.many1 . runParsing

manyTill :: Monad m => Parsing m a -> Parsing m b -> Parsing m [a]
manyTill x y = Parsing $ Parsec.manyTill (runParsing x) (runParsing y)

attempt :: Monad m => Parsing m a -> Parsing m a
attempt = Parsing . Parsec.try . runParsing

anyChar :: Monad m => Parsing m Char
anyChar = Parsing Parsec.anyChar

digit :: Monad m => Parsing m Char
digit = Parsing Parsec.digit

integer :: Monad m => Parsing m Integer
integer = readTrust =<< many1 digit

bounded :: Monad m => Char -> Parsing m ByteString
bounded c = char c *> bounded2 c

bounded2 :: Monad m => Char -> Parsing m ByteString
bounded2 c = toSL <$> manyTill anyChar (attempt $ char c)
