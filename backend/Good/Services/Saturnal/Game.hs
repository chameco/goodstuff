module Good.Services.Saturnal.Game where

import Good.Prelude

import Good.Services.Saturnal.Types
import Good.Services.Saturnal.Script

type Logging = IO

writeLog :: Text -> Logging ()
writeLog = putStrLn

makePlayer :: Text -> Player
makePlayer name = Player
  { playerName = name
  , playerColor = "#000000"
  , playerDeck = Deck
    { deckMain = []
    , deckSide = []
    }
  , playerResourceAlpha = Resource
    { resourceName = "α"
    , resourceQuantity = 0
    , resourceEventHandlers = []
    , resourceTemplates = []
    }
  , playerResourceBeta = Resource
    { resourceName = "β"
    , resourceQuantity = 0
    , resourceEventHandlers = []
    , resourceTemplates = []
    }
  , playerResourceGamma = Resource
    { resourceName = "γ"
    , resourceQuantity = 0
    , resourceEventHandlers = []
    , resourceTemplates = []
    }
  , playerResourceDelta = Resource
    { resourceName = "δ"
    , resourceQuantity = 0
    , resourceEventHandlers = []
    , resourceTemplates = []
    }
  , playerResources = []
  }

processTurn :: Turn -> Board -> Logging Board
processTurn t b = foldM (processMove (turnPlayer t)) b $ turnMoves t

processMove :: Text -> Board -> Move -> Logging Board
processMove player b m@MoveEntity{} =
  case locateEntity b (moveEntityID m) of
    Nothing -> pure b
    Just location ->
      case selectEntity b (moveEntityID m) location of
        Nothing -> pure b
        Just entity ->
          if entityOwner entity == player
          then act b entity (moveEntityAction m) location (moveEntityX m, moveEntityY m)
          else pure b

cellAt :: Board -> (Int, Int) -> Maybe Cell
cellAt b@Board{} (x, y) = do
  row <- index (boardCells b) y
  index row x

adjacentCells :: Board -> (Int, Int) -> [(Int, Int)]
adjacentCells b (x, y) = filter (\(x', y') -> x' >= 0 && x' < boardWidth b && y' >= 0 && y' < boardHeight b) points
  where points :: [(Int, Int)]
        points = if rem y 2 == 0
                 then [(x - 1, y - 1), (x, y - 2), (x, y - 1), (x - 1, y + 1), (x, y + 2), (x, y + 1)]
                 else [(x, y - 1), (x, y - 2), (x + 1, y - 1), (x, y + 1), (x, y + 2), (x + 1, y + 1)]

isAdjacent :: Board -> (Int, Int) -> (Int, Int) -> Bool
isAdjacent b start end = end `elem` adjacentCells b start

updateCell :: Board -> (Cell -> Cell) -> (Int, Int) -> Board
updateCell b f (x, y)
  | x < 0 || x >= boardWidth b || y < 0 || y >= boardHeight b = b
  | otherwise =
      let beforerows = take y $ boardCells b
          rows = drop y $ boardCells b
          row = headMay rows
          afterrows = drop 1 rows
      in case row of
           Nothing -> b
           Just r ->
             let beforecells = take x r
                 cells = drop x r
                 cell = headMay cells
                 aftercells = drop 1 cells
             in case cell of
                  Nothing -> b
                  Just c -> b { boardCells = beforerows <> [beforecells <> [f c] <> aftercells] <> afterrows }

removeEntity :: Board -> Entity -> (Int, Int) -> Board
removeEntity b e = updateCell b (\c -> c { cellEntities = filter ((/=entityID e) . entityID) $ cellEntities c })

spawnEntity :: Board -> Entity -> (Int, Int) -> Board
spawnEntity b e = updateCell b (\c -> c { cellEntities = e:cellEntities c })

locateEntity :: Board -> Text -> Maybe (Int, Int)
locateEntity b uuid = headMay $ foldr (\x acc -> case x of Just p -> p:acc; Nothing -> acc) [] flipped
  where numbered :: [([Cell], Int)]
        numbered = zip (boardCells b) [0..]
        cellhas :: Cell -> Bool
        cellhas c = any (\e -> entityID e == uuid) $ cellEntities c
        rowhas :: [(Cell, Int)] -> Maybe Int
        rowhas = fmap snd . headMay . filter (cellhas . fst)
        selected :: [(Maybe Int, Int)]
        selected = (\(row, i) -> (rowhas $ zip row [0..], i)) <$> numbered
        flipped :: [Maybe (Int, Int)]
        flipped = (\case (Just x, y) -> Just (x, y); _ -> Nothing) <$> selected

selectEntity :: Board -> Text -> (Int, Int) -> Maybe Entity
selectEntity b uuid p = cellAt b p >>= headMay . filter ((==uuid) . entityID) . cellEntities

moveEntity :: Board -> Text -> (Int, Int) -> (Int, Int) -> Board
moveEntity b uuid start end = case selectEntity b uuid start of
  Just e -> spawnEntity (removeEntity b e start) e end
  Nothing -> b

tagName :: Tag -> Text
tagName (Tag t) = t
tagName (TagData t _) = t

getTag :: [Tag] -> Text -> Maybe Tag
getTag [] _ = Nothing
getTag (t:xs) x | tagName t == x = Just t | otherwise = getTag xs x

hasTag :: [Tag] -> Text -> Bool
hasTag tags = isJust . getTag tags

setTag :: [Tag] -> Tag -> [Tag]
setTag [] t = [t]
setTag (t:xs) t' | tagName t == tagName t' = t':xs | otherwise = t:setTag xs t'

getEventHandler :: [EventHandler] -> Text -> Text
getEventHandler [] _ = ""
getEventHandler (EventHandler e s:es) e' | e == e' = s | otherwise = getEventHandler es e'

gameDict :: Dictionary
gameDict = [ ("moveEntity", \case
                 VCoords dest:VCoords pos:VEntity e:VBoard b:stack -> pure $ VBoard (moveEntity b (entityID e) pos dest):stack
                 _:_:_:_:_ -> badtype
                 _ -> underflow
             )
           , ("isAdjacent", \case
                 VCoords dest:VCoords pos:VBoard b:stack -> pure $ VBool (isAdjacent b pos dest):stack
                 _:_:_:_ -> badtype
                 _ -> underflow
             )
           ] <> defaultDict

act :: Board -> Entity -> Text -> (Int, Int) -> (Int, Int) -> Logging Board
act b e a pos dest =
  case compiled of
    Left err -> writeLog (mconcat ["Could not compile \"", handler, "\": ", toSL $ show err]) >> pure b
    Right s ->
      case collapse s [VBoard b] of
        Right (VBoard b':_) -> writeLog (mconcat ["Succesfully executed handler \"", handler, "\""]) >> pure b'
        Right _ -> writeLog "Handler did not return board" >> pure b
        Left err -> writeLog (mconcat ["Error in handler \"", handler, "\": ", err]) >> pure b
  where handler = getEventHandler (entityEventHandlers e) $ "do_" <> a
        dict = [ ("board", push $ VBoard b)
               , ("entity", push $ VEntity e)
               , ("pos", push $ VCoords pos)
               , ("dest", push $ VCoords dest)] <> gameDict
        compiled = compile dict handler
