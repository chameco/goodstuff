module Good.Services.Saturnal.Script where

import Good.Prelude

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P.C
import qualified Text.Megaparsec.Char.Lexer as P.C.L

import Good.Services.Saturnal.Types

data Value where
  VInt :: Int -> Value
  VString :: Text -> Value
  VBool :: Bool -> Value
  VQuote :: Script -> Value
  VBoard :: Board -> Value
  VEntity :: Entity -> Value
  VCoords :: (Int, Int) -> Value

instance Show Value where
  show (VInt x) = show x
  show (VString x) = show x
  show (VBool x) = show x
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
defaultDict = [ ("dup", \case
                    x:stack -> pure $ x:x:stack
                    _ -> underflow
                )
              , ("drop", \case
                    _:stack -> pure stack
                    _ -> underflow
                )
              , ("swap", \case
                    x:y:stack -> pure $ y:x:stack
                    _ -> underflow
                )
              , ("app", \case
                    VQuote s:stack -> collapse s stack
                    _:_ -> badtype
                    _ -> underflow
                )
              , ("true", push $ VBool True)
              , ("false", push $ VBool False)
              , ("if", \case
                    VBool c:VQuote t:VQuote e:stack -> if c then collapse t stack else collapse e stack
                    _:_:_:_ -> badtype
                    _ -> underflow
                )
              ]

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
