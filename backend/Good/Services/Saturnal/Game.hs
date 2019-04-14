module Good.Services.Saturnal.Game where

import Good.Prelude

import System.Random (randomRIO)

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)

import Numeric (showHex)

import Good.Services.Saturnal.Types
import Good.Services.Saturnal.Script

writeLog :: MonadIO m => Text -> m ()
writeLog = putStrLn

randomColor :: MonadIO m => m Text
randomColor = ("#"<>) . pack . flip showHex ""
  <$> liftIO (randomRIO (0, ((16 :: Int) ^ (6 :: Int)) :: Int))

makePlayer :: MonadIO m => Text -> Faction -> m Player
makePlayer name faction = do
  color <- randomColor
  pure Player
   { playerName = name
   , playerFaction = faction
   , playerColor = color
   , playerHand = []
   , playerDeck = Deck
     { deckMain = []
     , deckSide = []
     }
   , playerResourceAlpha = Resource
     { resourceID = "<alpha>"
     , resourceName = "α"
     , resourceQuantity = 0
     , resourceEventHandlers = []
     , resourceTemplates = []
     }
   , playerResourceBeta = Resource
     { resourceID = "<beta>"
     , resourceName = "β"
     , resourceQuantity = 0
     , resourceEventHandlers = []
     , resourceTemplates = []
     }
   , playerResourceGamma = Resource
     { resourceID = "<gamma>"
     , resourceName = "γ"
     , resourceQuantity = 0
     , resourceEventHandlers = []
     , resourceTemplates = []
     }
   , playerResourceDelta = Resource
     { resourceID = "<delta>"
     , resourceName = "δ"
     , resourceQuantity = 0
     , resourceEventHandlers = []
     , resourceTemplates = []
     }
   , playerResources = []
   }

--spawnPlayer :: (MonadIO m, MonadCatch m) => Board -> Text -> Faction -> m Board
--spawnPlayer b name faction = do
--  p <- makePlayer name faction
--  updatePlayer b (const p) name

processTurn :: (MonadIO m, MonadCatch m) => Turn -> Board -> m Board
processTurn t b = foldM (processMove (turnPlayer t)) b $ turnMoves t

processMove :: (MonadIO m, MonadCatch m) => Text -> Board -> Move -> m Board
processMove player b m@MoveEntity{} =
  case locateEntity b (moveEntityID m) of
    Nothing -> pure b
    Just location ->
      case selectEntity b (moveEntityID m) location of
        Nothing -> pure b
        Just entity ->
          if entityOwner entity == player
          then actEntity player b entity (moveEntityAction m) location (moveEntityX m, moveEntityY m)
          else pure b
processMove player b MoveDraw = pure $ drawCard b player
processMove player b m@MoveCard{} =
  case selectCardHand b (moveCardID m) player of
    Just c -> actCard player b c
    Nothing -> pure b

cellAt :: Board -> (Int, Int) -> Maybe Cell
cellAt b@Board{} (x, y) = do
  row <- index (boardCells b) y
  index row x

adjacentCells :: Board -> (Int, Int) -> [(Int, Int)]
adjacentCells b (x, y) = filter (\(x', y') -> x' >= 0 && x' < boardWidth b && y' >= 0 && y' < boardHeight b && (cellType <$> cellAt b (x', y')) /= Just CellBlack) points
  where points :: [(Int, Int)]
        points = if rem y 2 == 0
                 then [(x - 1, y - 1), (x, y - 2), (x, y - 1), (x - 1, y + 1), (x, y + 2), (x, y + 1)]
                 else [(x, y - 1), (x, y - 2), (x + 1, y - 1), (x, y + 1), (x, y + 2), (x + 1, y + 1)]

isAdjacent :: Board -> (Int, Int) -> (Int, Int) -> Bool
isAdjacent b start end = end `elem` adjacentCells b start

randomAdjacent :: MonadIO m => Board -> (Int, Int) -> m (Maybe (Int, Int))
randomAdjacent b pos = atMay adjacent <$> liftIO (randomRIO (0, length adjacent))
  where adjacent = adjacentCells b pos

selectPlayer :: Board -> Text -> Maybe Player
selectPlayer b player = headMay . filter ((== player) . playerName) $ boardPlayers b

updatePlayer :: Board -> (Player -> Player) -> Text -> Board
updatePlayer b f player =
  case selectPlayer b player of
    Just p -> b { boardPlayers = f p:filter ((/= player) . playerName) (boardPlayers b) }
    Nothing -> b

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

drawCard :: Board -> Text -> Board
drawCard b = updatePlayer b $ \p ->
  case headMay . deckMain $ playerDeck p of
    Just c -> p { playerDeck = (playerDeck p) { deckMain = drop 1 . deckMain $ playerDeck p }
                , playerHand = c:playerHand p
                }
    Nothing -> p

removeEntity :: Board -> Entity -> (Int, Int) -> Board
removeEntity b e = updateCell b (\c -> c { cellEntities = filter ((/= entityID e) . entityID) $ cellEntities c })

placeEntity :: Board -> Entity -> (Int, Int) -> Board
placeEntity b e = updateCell b (\c -> c { cellEntities = e:cellEntities c })

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
moveEntity b uuid start end =
  case selectEntity b uuid start of
    Just e -> placeEntity (removeEntity b e start) e end
    Nothing -> b

removeStructure :: Board -> Structure -> (Int, Int) -> Board
removeStructure b s = updateCell b (\c -> c { cellStructures = filter ((/= structureID s) . structureID) $ cellStructures c })

placeStructure :: Board -> Structure -> (Int, Int) -> Board
placeStructure b s = updateCell b (\c -> c { cellStructures = s:cellStructures c })

addResource :: Text -> Board -> Resource -> Board
addResource player b r = updatePlayer b (\p -> p { playerResources = r:playerResources p }) player

selectCardHand :: Board -> Text -> Text -> Maybe Card
selectCardHand b uuid player = selectPlayer b player >>= headMay . filter ((== uuid) . cardID) . playerHand 

addCard :: Text -> Board -> Card -> Board
addCard player b c = updatePlayer b (\p -> p { playerDeck = (playerDeck p) { deckMain = c:deckMain (playerDeck p) } }) player

addSidecard :: Text -> Board -> Sidecard -> Board
addSidecard player b c = updatePlayer b (\p -> p { playerDeck = (playerDeck p) { deckSide = c:deckSide (playerDeck p) } }) player

realize :: MonadIO m => Text -> Board -> Template -> (Int, Int) -> m Board
realize player b t c = do
  uuid <- liftIO (toText <$> nextRandom)
  case t of
    TemplateEmpty -> pure b
    TemplateEntity{} ->
      pure $ placeEntity b ( Entity { entityID = uuid
                                    , entityName = templateEntityName t
                                    , entityOwner = player
                                    , entityRank = templateEntityRank t
                                    , entityActions = templateEntityActions t
                                    , entityTags = templateEntityTags t
                                    , entityEventHandlers = templateEntityEventHandlers t
                                    , entityTemplates = templateEntityTemplates t
                                    }
                           ) c
    TemplateStructure{} ->
      pure $ placeStructure b ( Structure { structureID = uuid
                                          , structureName = templateStructureName t
                                          , structureOwner = player
                                          , structureActions = templateStructureActions t
                                          , structureTags = templateStructureTags t
                                          , structureEventHandlers = templateStructureEventHandlers t
                                          , structureTemplates = templateStructureTemplates t
                                          }
                              ) c
    TemplateResource{} ->
      pure $ addResource player b ( Resource { resourceID = uuid
                                             , resourceName = templateResourceName t
                                             , resourceQuantity = 0
                                             , resourceEventHandlers = templateResourceEventHandlers t
                                             , resourceTemplates = templateResourceTemplates t
                                             }
                                  )
    TemplateCard{} ->
      pure $ addCard player b ( Card { cardID = uuid
                                     , cardName = templateCardName t
                                     , cardCost = templateCardCost t
                                     , cardEffect = templateCardEffect t
                                     , cardTemplates = templateCardTemplates t
                                     }
                              )
    TemplateSidecard{} ->
      pure $ addSidecard player b ( Sidecard { sidecardID = uuid
                                             , sidecardName = templateSidecardName t
                                             , sidecardEffect = templateSidecardEffect t
                                             , sidecardTemplates = templateSidecardTemplates t
                                             }
                                  )

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

getTemplate :: [Template] -> Text -> Template
getTemplate [] _ = TemplateEmpty
getTemplate (t:ts) n | templateName t == n = t | otherwise = getTemplate ts n

getFactionTemplate :: Board -> Text -> Text -> Template
getFactionTemplate b player n = maybe TemplateEmpty (flip getTemplate n . factionTemplates . playerFaction) $ selectPlayer b player 

gameDict :: forall (m :: Type -> Type). (MonadIO m, MonadCatch m) => Dictionary m
gameDict = [ ( "drawCard", \case
                 VString player:VBoard b:stack -> pure $ VBoard (drawCard b player):stack
                 _:_:_ -> badtype
                 _ -> underflow
             )
           , ( "moveEntity", \case
                 VCoords dest:VCoords pos:VEntity e:VBoard b:stack -> pure $ VBoard (moveEntity b (entityID e) pos dest):stack
                 _:_:_:_:_ -> badtype
                 _ -> underflow
             )
           , ( "isAdjacent", \case
                 VCoords dest:VCoords pos:VBoard b:stack -> pure $ VBool (isAdjacent b pos dest):stack
                 _:_:_:_ -> badtype
                 _ -> underflow
             )
           , ( "randomAdjacent", \case
                 VCoords pos:VBoard b:stack -> randomAdjacent b pos >>= \case
                   Just c -> pure $ VCoords c:stack
                   Nothing -> pure $ VUnit:stack
                 _:_:_ -> badtype
                 _ -> underflow
             )
           , ( "realize", \case
                 VCoords pos:VTemplate t:VString player:VBoard b:stack -> (:stack) . VBoard <$> realize player b t pos
                 _:_:_:_:_ -> badtype
                 _ -> underflow
             )
           , ( "getTemplate", \case
                 VString t:VEntity e:stack -> pure $ VTemplate (getTemplate (entityTemplates e) t):stack
                 VString t:VStructure s:stack -> pure $ VTemplate (getTemplate (structureTemplates s) t):stack
                 VString t:VResource r:stack -> pure $ VTemplate (getTemplate (resourceTemplates r) t):stack
                 VString t:VCard c:stack -> pure $ VTemplate (getTemplate (cardTemplates c) t):stack
                 VString t:VSidecard c:stack -> pure $ VTemplate (getTemplate (sidecardTemplates c) t):stack
                 _:_:_ -> badtype
                 _ -> underflow
             )
           , ( "getFactionTemplate", \case
                 VString t:VString player:VBoard b:stack -> pure $ VTemplate (getFactionTemplate b player t):stack
                 _:_:_:_ -> badtype
                 _ -> underflow
             )
           ] <> defaultDict

actHelper :: forall (m :: Type -> Type) (a :: Type). (MonadIO m, MonadCatch m) =>
  Text -> Board ->
  a -> Text -> (a -> [EventHandler]) -> (a -> Value m) ->
  Text -> (Int, Int) -> (Int, Int) -> m Board
actHelper player b e w h v a pos dest =
  try ( do compiled <- compile dict handler
           result <- collapse compiled [VBoard b]
           case result of
             (VBoard b':_) -> writeLog (mconcat ["Succesfully executed handler \"", handler, "\""]) >> pure b'
             _ -> writeLog "Handler did not return board" >> pure b
      ) >>=
  \case
    Left (ScriptError err) -> writeLog (mconcat ["Error in handler \"", handler, "\": ", err]) >> pure b
    Right board -> pure board
  where handler = getEventHandler (h e) $ "do_" <> a
        dict = [ ("board", push $ VBoard b)
               , (w, push $ v e)
               , ("pos", push $ VCoords pos)
               , ("dest", push $ VCoords dest)
               , ("player", push $ VString player)
               ] <> gameDict

actEntity :: forall (m :: Type -> Type). (MonadIO m, MonadCatch m) => Text -> Board -> Entity -> Text -> (Int, Int) -> (Int, Int) -> m Board
actEntity player b e = actHelper player b e "entity" entityEventHandlers VEntity

actStructure :: forall (m :: Type -> Type). (MonadIO m, MonadCatch m) => Text -> Board -> Structure -> Text -> (Int, Int) -> (Int, Int) -> m Board
actStructure player b e = actHelper player b e "structure" structureEventHandlers VStructure

actResource :: forall (m :: Type -> Type). (MonadIO m, MonadCatch m) => Text -> Board -> Resource -> Text -> (Int, Int) -> (Int, Int) -> m Board
actResource player b e = actHelper player b e "resource" resourceEventHandlers VResource

-- | Resolve a card effect
actCard :: (MonadIO m, MonadCatch m) => Text -> Board -> Card -> m Board
actCard player b c =
  try ( do compiled <- compile dict $ cardEffect c
           result <- collapse compiled [VBoard b]
           case result of
             (VBoard b':_) -> writeLog (mconcat ["Succesfully executed handler \"", cardEffect c, "\""]) >> pure b'
             _ -> writeLog "Handler did not return board" >> pure b
      ) >>=
  \case
    Left (ScriptError err) -> writeLog (mconcat ["Error in handler \"", cardEffect c, "\": ", err]) >> pure b
    Right board -> pure board
  where dict = [ ("board", push $ VBoard b)
               , ("card", push $ VCard c)
               , ("player", push $ VString player)
               ] <> gameDict

civLike :: Faction
civLike = Faction "<civ-like>" "civLike"
  [ TemplateEntity
    { templateEntityName = "Settler"
    , templateEntityRank = 3
    , templateEntityActions =
      [ ActionDescription "move" "Move"
      , ActionDescription "settle" "Settle"
      ]
    , templateEntityTags = []
    , templateEntityEventHandlers =
      [ EventHandler "do_move" "[] [entity pos dest moveEntity] board pos dest isAdjacent if"
      , EventHandler "do_settle" "player entity \"City\" getTemplate pos realize"
      ]
    , templateEntityTemplates =
      [ TemplateStructure
        { templateStructureName = "City"
        , templateStructureActions =
          [ ActionDescription "spawn_settler" "Spawn Settler"
          , ActionDescription "spawn_warrior" "Spawn Warrior"
          ]
        , templateStructureTags = []
        , templateStructureEventHandlers =
          [ EventHandler "do_spawn_settler" "player board player \"Settler\" getFactionTemplate pos realize"
          , EventHandler "do_spawn_warrior" "player structure \"Warrior\" getTemplate pos realize"
          ]
        , templateStructureTemplates =
          [ TemplateEntity
            { templateEntityName = "Warrior"
            , templateEntityRank = 4
            , templateEntityActions =
              [ ActionDescription "move" "Move"
              ]
            , templateEntityTags = []
            , templateEntityEventHandlers =
              [ EventHandler "do_move" "[] [entity pos dest moveEntity] board pos dest isAdjacent if"
              ]
            , templateEntityTemplates = []
            }
          ]
        }
      ]
    }
  ]
