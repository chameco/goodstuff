module Good.Services.Sam where

import Good.Prelude

import qualified Data.ByteString.Char8 as BS

import Text.Regex.PCRE.Heavy

import Good.Architecture.Parsing

data Single = Character Integer
            | Line Integer
            | Find ByteString
            | End
            | Dot
            deriving (Show, Eq)

data Multiple = After Single Single
              | Before Single Single
              | Inclusive Single Single
              | Exclusive Single Single
              deriving (Show, Eq)

data Address = Simple Single
             | Compound Multiple
             deriving (Show, Eq)

data Command = Set Address
             | Change ByteString
             | Insert ByteString
             | Append ByteString
             | Delete
             | Each ByteString Command
             | Block [Command]
             deriving (Show, Eq)

parseSingle :: Monad m => Parsing m Single
parseSingle = Character <$> (char '#' *> integer)
              <|> Line <$> integer
              <|> Find <$> bounded '/'
              <|> End <$ char '$'
              <|> Dot <$ char '.'

parseMultiple :: Monad m => Parsing m Multiple
parseMultiple = attempt (After Dot <$> (char '+' *> parseSingle))
                <|> attempt (Before End <$> (char '-' *> parseSingle))
                <|> attempt (After Dot End <$ char '+')
                <|> attempt (Before Dot End <$ char '-')
                <|> attempt (After <$> (parseSingle <* char '+') <*> parseSingle)
                <|> attempt (Before <$> (parseSingle <* char '-') <*> parseSingle)
                <|> attempt (flip After End <$> (parseSingle <* char '+'))
                <|> attempt (flip Before End <$> (parseSingle <* char '-'))

parseAddress :: Monad m => Parsing m Address
parseAddress = attempt (Simple <$> parseSingle) <|> attempt (Compound <$> parseMultiple)

parseCommand :: Monad m => Parsing m Command
parseCommand = Set <$> parseAddress
               <|> Change <$> (char 'c' *> bounded '/')
               <|> Insert <$> (char 'i' *> bounded '/')
               <|> Append <$> (char 'a' *> bounded '/')
               <|> Delete <$ char 'd'
               <|> Each <$> (char 'x' *> bounded '/') <*> parseCommand
               <|> Block <$> (char '{' *> parseCommands <* char '}')

parseCommands :: Monad m => Parsing m [Command]
parseCommands = whitespace *> many (parseCommand <* whitespace)

newtype File = File { fileLines :: [ByteString]
                    } deriving Show

type Position = (Integer, Integer)
type Range = (Position, Position)
type State = (File, Range)

positionIndex :: File -> Position -> Integer
positionIndex f (l, c) = go 0 0 $ fileLines f
    where go :: Integer -> Integer -> [ByteString] -> Integer
          go acc _ [] = acc
          go acc line (x:xs) = if line == l then if c < fromIntegral (length x) then acc + c else acc + fromIntegral (length x) - 1
                                            else go (acc + fromIntegral (length x)) (succ line) xs

charsAfter :: File -> Position -> Integer -> Position
charsAfter f (pl, pc) n = go (n + pc) pl 0 . drop (fromIntegral pl) $ fileLines f
    where go :: Integer -> Integer -> Integer -> [ByteString] -> Position
          go _ line _ [] = (line - 1, linelength f (line - 1) - 1)
          go t line acc (x:xs) = let l = acc + fromIntegral (length x)
                                 in if | t < 0 -> if line > 0 then charsAfter f (pred line, 0) (t + fromIntegral (length x) - 1) else (0, 0)
                                       | t < l -> (line, t - acc)
                                       | otherwise -> go t (succ line) l xs

linelength :: File -> Integer -> Integer
linelength f n = go 0 $ fileLines f
    where go :: Integer -> [ByteString] -> Integer
          go _ [] = 0
          go line (x:xs) | line == n = fromIntegral (length x) | otherwise = go (succ line) xs

matches :: ByteString -> ByteString -> [(Integer, Integer)]
matches r s = fromMaybe [] $ do
        reg <- case compileM r [] of Right x -> Just x; _ -> Nothing
        ms <- rawMatch reg s 0 []
        pure $ (fromIntegral *** fromIntegral) <$> ms

simpleRange :: Single -> State -> Range
simpleRange (Character n) (file, _) = let p = charsAfter file (0, 0) n in (p, p)
simpleRange (Line n) _ = ((n, 0), (n + 1, 0))
simpleRange (Find r) (file, dot) = fromMaybe dot $ do
        (s, e) <- headMay $ matches r (mconcat $ fileLines file)
        pure (charsAfter file (0, 0) $ fromIntegral s, charsAfter file (0, 0) $ fromIntegral e)
simpleRange End (file, _) = let l = pred . fromIntegral . length $ fileLines file in ((l, 0), (l, linelength file l))
simpleRange Dot (_, dot) = dot

compoundRange :: Multiple -> State -> Range
compoundRange (After base (Character c)) s@(file, _) = let p = charsAfter file (snd $ simpleRange base s) c in (p, p)
compoundRange (After base (Line l)) s = simpleRange (Line (l + fst (fst (simpleRange base s)))) s
compoundRange (After base (Find r)) s@(file, dot) = fromMaybe dot $ do
        (start, end) <- headMay $ matches r (drop pos . mconcat $ fileLines file)
        pure (charsAfter file (0, 0) $ fromIntegral start, charsAfter file (0, 0) $ fromIntegral end)
    where pos = fromIntegral . positionIndex file . snd $ simpleRange base s
compoundRange (After _ End) (_, dot) = dot
compoundRange (After _ Dot) (_, dot) = dot
compoundRange (Before base (Character c)) s@(file, _) = let p = charsAfter file (fst $ simpleRange base s) (c * (-1)) in (p, p)
compoundRange (Before base (Line l)) s = simpleRange (Line (l - fst (fst (simpleRange base s)))) s
compoundRange (Before base (Find r)) s@(file, dot) = fromMaybe dot $ do
        (start, end) <- lastMay $ matches r (take pos . mconcat $ fileLines file)
        pure (charsAfter file (0, 0) $ fromIntegral start, charsAfter file (0, 0) $ fromIntegral end)
    where pos = fromIntegral . positionIndex file . snd $ simpleRange base s
compoundRange (Before _ End) (_, dot) = dot
compoundRange (Before _ Dot) (_, dot) = dot
compoundRange (Inclusive start end) s = (fst $ simpleRange start s, snd $ simpleRange end s)
compoundRange (Exclusive start end) s = (snd $ simpleRange start s, fst $ simpleRange end s)

addressRange :: Address -> State -> Range
addressRange (Simple a) s = simpleRange a s
addressRange (Compound a) s = case compoundRange a s of (b, e) -> if b < e then (b, e) else (e, b)

fetch :: State -> ByteString
fetch (file, (s, e)) = take (end - start) $ drop start $ mconcat $ fileLines file
    where start = fromIntegral $ positionIndex file s
          end = fromIntegral $ positionIndex file e

change :: ByteString -> State -> State
change t (file, ((sl, sc), (el, ec))) = (File $ before ++ new ++ after, ((sl, sc), (sl + fromIntegral (length new) - 1, lastlen - fromIntegral (length a))))
    where (before, sline:_) = splitAt (fromIntegral sl) $ fileLines file
          (b, _) = BS.splitAt (fromIntegral sc) sline
          (_, eline:after) = splitAt (fromIntegral el) $ fileLines file
          (_, a) = BS.splitAt (fromIntegral ec) eline
          new = fromMaybe [] $ initMay ((<> "\n") <$> BS.split '\n' (b <> t <> a))
          lastlen = maybe 0 (fromIntegral . length) $ lastMay new

executeAll :: [Command] -> State -> State
executeAll = flip . foldl' $ flip execute

execute :: Command -> State -> State
execute (Set a) s@(file, _) = (file, addressRange a s)
execute (Change t) s = change t s
execute (Insert t) (file, (start, _)) = change t (file, (start, start))
execute (Append t) (file, (_, end)) = change t (file, (end, end))
execute Delete s = change "" s
execute (Each r c) s = go 0 s
    where go :: Integer -> State -> State
          go x (f, d) = fromMaybe (f, d) $ do
                  reg <- case compileM r [] of Right y -> Just y; _ -> Nothing
                  ms <- rawMatch reg body (fromIntegral x) []
                  (start, end) <- lastMay ms
                  range <- pure (charsAfter f (0, 0) $ fromIntegral start, charsAfter f (0, 0) $ fromIntegral end)
                  state <- pure $ execute c (f, range)
                  pure $ go (fromIntegral end) state
              where body = mconcat $ fileLines f
execute (Block cs) s = executeAll cs s
