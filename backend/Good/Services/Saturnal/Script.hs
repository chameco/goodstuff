module Good.Services.Saturnal.Script where

import Good.Prelude

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P.C
import qualified Text.Megaparsec.Char.Lexer as P.C.L

import Good.Services.Saturnal.Types

data Value where
  VInt :: Int -> Value
  VString :: Text -> Value
  VQuote :: Script -> Value
  VBoard :: Board -> Value
  VEntity :: Entity -> Value
  VCoords :: (Int, Int) -> Value

instance Show Value where
  show (VInt x) = show x
  show (VString x) = show x
  show (VQuote _) = "<quote>"
  show (VBoard _) = "<board>"
  show (VEntity e) = mconcat ["<entity: ", toSL $ entityID e, ">"]
  show (VCoords c) = mconcat ["<coords: ", show c, ">"]

type Stack = [Value]

type Instruction = [Value] -> Either Text [Value]

type Dictionary = [(Text, Instruction)]

type Script = [Instruction]

lookupDict :: Dictionary -> Text -> Instruction
lookupDict [] t = const . Left $ mconcat ["Instruction \"", t, "\" not found"]
lookupDict ((x, f):xs) t | x == t = f | otherwise = lookupDict xs t

collapse :: Script -> Instruction
collapse [] = pure 
collapse (f:s) = collapse s >=> f

underflow :: forall (a :: Type). Either Text a
underflow = Left "Stack underflow"

badtype :: forall (a :: Type). Either Text a
badtype = Left "Bad type"

push :: Value -> Instruction
push x stack = pure (x:stack)

defaultDict :: Dictionary
defaultDict = [ ("dup", dup)
              , ("drop", drp)
              , ("swap", swp)
              , ("app", app)
              ]
  where dup :: Instruction
        dup (x:stack) = pure (x:x:stack)
        dup _ = underflow
        drp :: Instruction
        drp (_:stack) = pure stack
        drp _ = underflow
        swp :: Instruction
        swp (x:y:stack) = pure (y:x:stack)
        swp _ = underflow
        app :: Instruction
        app (x:stack) = case x of
          VQuote s -> collapse s stack
          _ -> badtype
        app _ = underflow

compile :: Dictionary -> Text -> Either (P.ParseError (P.Token Text) Text) Script
compile d = P.parse (parserScript d) ""

type Parser = P.Parsec Text Text

parserScript :: Dictionary -> Parser Script
parserScript d = reverse <$> P.many (P.C.space *> parserInstruction d <* P.C.space)

parserInstruction :: Dictionary -> Parser Instruction
parserInstruction d = lookupDict d . toSL <$> ((:) <$> P.C.letterChar <*> P.many P.C.alphaNumChar)
                      <|> push <$> parserValue d

parserValue :: Dictionary -> Parser Value
parserValue d = VInt <$> P.C.L.decimal
                <|> VString . toSL <$> (P.C.char '"' *> P.manyTill P.C.L.charLiteral (P.C.char '"'))
                <|> VQuote <$> (P.C.char '[' *> parserScript d <* P.C.char ']') 
