module Good.Services.Saturnal.Script where

import Good.Prelude

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P.C
import qualified Text.Megaparsec.Char.Lexer as P.C.L

import Good.Services.Saturnal.Types

newtype ScriptError = ScriptError Text deriving Show
instance Exception ScriptError

data Value m where
  VUnit :: Value m
  VInt :: Int -> Value m
  VString :: Text -> Value m
  VBool :: Bool -> Value m
  VQuote :: Script m -> Value m
  VBoard :: Board -> Value m
  VTemplate :: Template -> Value m
  VEntity :: Entity -> Value m
  VStructure :: Structure -> Value m
  VResource :: Resource -> Value m
  VCard :: Card -> Value m
  VSidecard :: Sidecard -> Value m
  VCoords :: (Int, Int) -> Value m

instance Show (Value m) where
  show VUnit = "Nothing"
  show (VInt x) = show x
  show (VString x) = show x
  show (VBool x) = show x
  show (VQuote _) = "<quote>"
  show (VBoard _) = "<board>"
  show (VTemplate _) = "<template>"
  show (VEntity e) = mconcat ["<entity: ", toSL $ entityID e, ">"]
  show (VStructure s) = mconcat ["<struture: ", toSL $ structureID s, ">"]
  show (VResource r) = mconcat ["<resource: ", toSL $ resourceID r, ">"]
  show (VCard c) = mconcat ["<card: ", toSL $ cardID c, ">"]
  show (VSidecard c) = mconcat ["<sidecard: ", toSL $ sidecardID c, ">"]
  show (VCoords c) = mconcat ["<coords: ", show c, ">"]

type Stack m = [Value m]

type Instruction m = [Value m] -> m [Value m]

type Dictionary m = [(Text, Instruction m)]

type Script m = [Instruction m]

lookupDict :: MonadThrow m => Dictionary m -> Text -> Instruction m
lookupDict [] t = const . throwM . ScriptError $ mconcat ["Instruction \"", t, "\" not found"]
lookupDict ((x, f):xs) t | x == t = f | otherwise = lookupDict xs t

collapse :: Monad m => Script m -> Instruction m
collapse [] = pure
collapse (f:s) = collapse s >=> f

underflow :: MonadThrow m => m a
underflow = throwM $ ScriptError "Stack underflow"

badtype :: MonadThrow m => m a
badtype = throwM $ ScriptError "Bad type"

push :: Applicative m => Value m -> Instruction m
push x stack = pure (x:stack)

defaultDict :: MonadThrow m => Dictionary m
defaultDict = [ ( "dup", \case
                    x:stack -> pure $ x:x:stack
                    _ -> underflow
                )
              , ( "drop", \case
                    _:stack -> pure stack
                    _ -> underflow
                )
              , ( "swap", \case
                    x:y:stack -> pure $ y:x:stack
                    _ -> underflow
                )
              , ( "app", \case
                    VQuote s:stack -> collapse s stack
                    _:_ -> badtype
                    _ -> underflow
                )
              , ( "true", push $ VBool True)
              , ( "false", push $ VBool False)
              , ( "qiu", \case
                    VUnit:_ -> throwM $ ScriptError "Intentional quit"
                    stack -> pure stack
                )
              , ( "qif", \case
                    VBool False:_ -> throwM $ ScriptError "Intentional quit"
                    stack -> pure stack
                )
              , ("if", \case
                    VBool c:VQuote t:VQuote e:stack -> if c then collapse t stack else collapse e stack
                    _:_:_:_ -> badtype
                    _ -> underflow
                )
              ]

compile :: MonadThrow m => Dictionary m -> Text -> m (Script m)
compile d s =
  case P.parse (parserScript d) "" s of
    Left err -> throwM . ScriptError . toSL $ show err
    Right x -> pure x

type Parser = P.Parsec Text Text

parserScript :: MonadThrow m => Dictionary m -> Parser (Script m)
parserScript d = reverse <$> P.many (P.C.space *> parserInstruction d <* P.C.space)

parserInstruction :: MonadThrow m => Dictionary m -> Parser (Instruction m)
parserInstruction d = lookupDict d . toSL <$> ((:) <$> P.C.letterChar <*> P.many P.C.alphaNumChar)
                      <|> push <$> parserValue d

parserValue :: MonadThrow m => Dictionary m -> Parser (Value m)
parserValue d = VInt <$> P.C.L.decimal
                <|> VString . toSL <$> (P.C.char '"' *> P.manyTill P.C.L.charLiteral (P.C.char '"'))
                <|> VQuote <$> (P.C.char '[' *> parserScript d <* P.C.char ']') 
