module Good.Services.TL7 where

import Good.Prelude

import System.Random

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Good.Interfaces.Web

api :: Serving IO ()
api = handling (Get "/tl7/generate") $ do
  cs <- mconcat <$> characters 4
  pure . Markup . H.docTypeHtml $ mconcat
    [ H.head $ mconcat
      [ H.title "Character Generator"
      , H.link ! A.rel "stylesheet" ! A.href "https://chame.co/style/style.css"
      ]
    , H.body cs
    ]

characters :: MonadIO m => Integer -> m [H.Html]
characters n | n <= 0 = pure []
             | otherwise = (:) <$> character <*> characters (pred n)

character :: MonadIO m => m H.Html 
character = do
  background <- rollBackground
  appearance <- rollAppearance
  trait <- rollTrait
  personality <- rollPersonality
  possessions <- roll (Sides 6) >>= items
  pure $ mconcat
    [ H.div ! A.class_ "outline-2" $ H.ul $ mconcat
      [ H.li $ mconcat [H.strong "Background: ", H.toHtml background]
      , H.li $ mconcat [H.strong "Appearance: ", H.toHtml appearance]
      , H.li $ mconcat [H.strong "Trait: ", H.toHtml trait]
      , H.li $ mconcat [H.strong "Personality: ", H.toHtml personality]
      , H.li $ mconcat [H.strong "Possessions: ", H.ul $ mconcat possessions]
      ]
    ]

items :: MonadIO m => Integer -> m [H.Html]
items n | n <= 0 = pure []
        | otherwise = (:) <$> item <*> items (pred n)

item :: MonadIO m => m H.Html
item = H.li . H.toHtml <$> rollItem

data Die where
  Sides :: Integer -> Die
  Constant :: Integer -> Die
  Both :: Die -> Die -> Die
  Many :: Integer -> Die -> Die
  deriving (Show, Eq)

roll :: MonadIO m => Die -> m Integer
roll (Sides x) = liftIO $ getStdRandom (randomR (1, x))
roll (Constant x) = pure x
roll (Both d d') = (+) <$> roll d <*> roll d'
roll (Many i d) | i <= 0 = pure 0
                | otherwise = (+) <$> roll d <*> roll (Many (pred i) d)

rollBackground :: MonadIO m => m Text
rollBackground = do
  start <- roll (Sides 6)
  case start of
    1 -> rollScum
    2 -> rollHardLabor
    3 -> rollService
    4 -> rollGoodCleanLiving
    5 -> rollArtisan
    _ -> rollMercantile
  where
    rollScum = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "cockroach herder"
        2 -> pure "rickshaw runner"
        3 -> pure "pit fighter"
        4 -> pure "dredger"
        5 -> pure "gutter thug"
        _ -> rollHardLabor
    rollHardLabor = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "sailor"
        2 -> pure "hauler"
        3 -> pure "dockworker"
        4 -> pure "stonemason"
        5 -> pure "cultist"
        _ -> rollService
    rollService = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "servant"
        2 -> pure "cook"
        3 -> pure "kennelmaster"
        4 -> pure "chauffeur"
        5 -> pure "porter"
        _ -> rollGoodCleanLiving
    rollGoodCleanLiving = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "mercenary"
        2 -> pure "landed farmer"
        3 -> pure "ferryman"
        4 -> pure "second son"
        5 -> pure "priest"
        _ -> rollArtisan
    rollArtisan = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "carpenter"
        2 -> pure "jeweler"
        3 -> pure "chandler"
        4 -> pure "hatter"
        5 -> pure "silversmith"
        _ -> rollMercantile
    rollMercantile = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "restaurateur"
        2 -> pure "shipping tycoon"
        3 -> pure "fence"
        4 -> pure "disgraced scion"
        5 -> pure "slaver"
        _ -> rollIdleWealthy
    rollIdleWealthy = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "inheritor"
        2 -> pure "patriarch/matriarch"
        3 -> pure "financier"
        4 -> pure "philanthropic retiree"
        5 -> pure "patron"
        _ -> rollNobility
    rollNobility = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "high priest"
        2 -> pure "princess"
        3 -> pure "clan head"
        4 -> pure "vizier"
        5 -> pure "cartel boss"
        _ -> pure "general/admiral"

rollAppearance :: MonadIO m => m Text
rollAppearance = do
  start <- roll (Sides 6)
  case start of
    1 -> rollOof
    2 -> rollUnflattering
    3 -> rollNotGreat
    4 -> rollWhoAmIKidding
    5 -> rollNoneOfTheseAreGood
    _ -> rollTable6
  where
    rollOof = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "elephant man"
        2 -> pure "droopy eyes"
        3 -> pure "three chins"
        4 -> pure "haggard"
        5 -> pure "passes for burn victim"
        _ -> rollUnflattering
    rollUnflattering = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "effete"
        2 -> pure "awkward proportions"
        3 -> pure "two chins"
        4 -> pure "hirsute"
        5 -> pure "ugly"
        _ -> rollNotGreat
    rollNotGreat = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "fish eyes"
        2 -> pure "sickeningly thin"
        3 -> pure "one chins"
        4 -> pure "corpulent"
        5 -> pure "rat face"
        _ -> rollWhoAmIKidding
    rollWhoAmIKidding = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "hunchback"
        2 -> pure "obese"
        3 -> pure "zero chins"
        4 -> pure "balding"
        5 -> pure "wrinkled"
        _ -> rollNoneOfTheseAreGood
    rollNoneOfTheseAreGood = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "resembles disinterred corpse, a decade later"
        2 -> pure "conehead"
        3 -> pure "completely average"
        4 -> pure "face looks kinda weird"
        5 -> pure "cauliflower ears"
        _ -> rollTable6
    rollTable6 = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "holy shit that's his actual face"
        2 -> pure "Brendan Fraser"
        3 -> pure "cliche \"bar tough guy\""
        4 -> pure "actually looks pretty good"
        5 -> pure "fat fat fat"
        _ -> rollTheNextOne
    rollTheNextOne = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "twink"
        2 -> pure "has a cool moustache and hangs around schools a lot"
        3 -> pure "heavilly tattooed"
        4 -> pure "bulging eyes"
        5 -> pure "barrel chest"
        _ -> rollBottomText
    rollBottomText = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "hands, feet, and head too big for body"
        2 -> pure "incredibly attractive"
        3 -> pure "sqrt(-1) chins, missing entire jaw"
        4 -> pure "powerful frame"
        5 -> pure "branded on forehead"
        _ -> rollOof

rollTrait :: MonadIO m => m Text
rollTrait = do
  start <- roll (Sides 6)
  case start of
    1 -> rollHead
    2 -> rollShoulders
    3 -> rollKnees
    4 -> rollAnd
    5 -> rollToes
    _ -> rollKneesRedux
  where
    rollHead = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "glass eye"
        2 -> pure "missing ear"
        3 -> pure "forked tongue"
        4 -> pure "no teeth"
        5 -> pure "entirely hairless"
        _ -> rollShoulders
    rollShoulders = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "missing arm"
        2 -> pure "hook for hand"
        3 -> pure "major scar across torso"
        4 -> pure "patchwork skin"
        5 -> pure "fingerprints burned off"
        _ -> rollKnees
    rollKnees = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "peg leg"
        2 -> pure "irritating shin splints"
        3 -> pure "diabetic"
        4 -> pure "limp"
        5 -> pure "eunuch"
        _ -> rollAnd
    rollAnd = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "profane symbol tattooed on forehead"
        2 -> pure "hands swapped, right hand on left arm"
        3 -> pure "nine fingers total"
        4 -> pure "nine fingers on each hand"
        5 -> pure "missing buttock"
        _ -> rollToes
    rollToes = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "foot made of solid silver"
        2 -> pure "wooden brace supports leg"
        3 -> pure "broken nose"
        4 -> pure "giant legs, tiny body otherwise"
        5 -> pure "comically flat feet"
        _ -> rollKneesRedux
    rollKneesRedux = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "terrible haircut"
        2 -> pure "practically mummified"
        3 -> pure "frail"
        4 -> pure "no fingernails"
        5 -> pure "arthritic"
        _ -> rollAndRedux
    rollAndRedux = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "no vocal cords"
        2 -> pure "calloused knuckles"
        3 -> pure "wheezes, easily winded"
        4 -> pure "very bad teeth"
        5 -> pure "inflexible, limited range of movement in limbs"
        _ -> rollToesRedux
    rollToesRedux = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "large horn protrudes from forehead"
        2 -> pure "unremarkable"
        3 -> pure "second head with own personality"
        4 -> pure "intimidating musculature"
        5 -> pure "stands eight feet tall"
        _ -> pure "angelic halo"

rollPersonality :: MonadIO m => m Text
rollPersonality = do
  start <- roll (Sides 6)
  case start of
    1 -> rollENFP
    2 -> rollINTJ
    3 -> rollHTML
    4 -> rollHELP
    5 -> rollMBTI
    _ -> rollMCHN
  where
    rollENFP = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "foolhardy"
        2 -> pure "aloof"
        3 -> pure "sniveling"
        4 -> pure "hot-tempered"
        5 -> pure "vengeful"
        _ -> rollINTJ
    rollINTJ = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "callous"
        2 -> pure "greedy"
        3 -> pure "easily insulted"
        4 -> pure "moron"
        5 -> pure "well-mannered"
        _ -> rollHTML
    rollHTML = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "brooding"
        2 -> pure "purehearted"
        3 -> pure "prideful"
        4 -> pure "lunatic"
        5 -> pure "calculating"
        _ -> rollHELP
    rollHELP = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "slovenly"
        2 -> pure "caustic"
        3 -> pure "honor bound"
        4 -> pure "devoted to family"
        5 -> pure "judgemental"
        _ -> rollMBTI
    rollMBTI = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "bipolar"
        2 -> pure "stick-in-the-mud"
        3 -> pure "religious"
        4 -> pure "lazy"
        5 -> pure "glutton"
        _ -> rollMCHN
    rollMCHN = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "irascible"
        2 -> pure "noble spirit"
        3 -> pure "autist"
        4 -> pure "hateful"
        5 -> pure "lustful"
        _ -> rollBROK
    rollBROK = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "schizophrenic"
        2 -> pure "craven"
        3 -> pure "outgoing"
        4 -> pure "addict"
        5 -> pure "celibate"
        _ -> rollLMAO
    rollLMAO = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "modest"
        2 -> pure "good mediator"
        3 -> pure "poor impulse control"
        4 -> pure "beautiful soul"
        5 -> pure "brave"
        _ -> pure "genius"

rollItem :: MonadIO m => m Text
rollItem = do
  start <- roll (Sides 6)
  case start of
    1 -> rollWaste
    2 -> rollTools
    3 -> rollUtensils
    4 -> rollHeirlooms
    5 -> rollHandicrafts
    _ -> rollTrinkets
  where
    rollWaste = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "trained rat"
        2 -> pure "a handful of broken teeth"
        3 -> pure "flask of gutter oil"
        4 -> pure "a dozen meters of sausage casing (unknown origin)"
        5 -> pure "dull shiv"
        _ -> rollTools
    rollTools = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "hacksaw"
        2 -> pure "hod of bricks"
        3 -> pure "bag of flour"
        4 -> pure "shovel"
        5 -> pure "hammer and a dozen nails"
        _ -> rollUtensils
    rollUtensils = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "leash and collar"
        2 -> pure "chalk"
        3 -> pure "greasy side of meat (unknown origin)"
        4 -> pure "detached chariot wheel (spiked)"
        5 -> pure "a dozen meters of curtains"
        _ -> rollHeirlooms
    rollHeirlooms = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "spyglass"
        2 -> pure "compass"
        3 -> pure "gondolier's pole"
        4 -> pure "rope ladder"
        5 -> pure "dated map"
        _ -> rollHandicrafts
    rollHandicrafts = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "lantern and a dozen flasks of oil"
        2 -> pure "pair of oars"
        3 -> pure "a dozen caltrops"
        4 -> pure "a dozen iron spikes"
        5 -> pure "a dozen meters of rope"
        _ -> rollTrinkets
    rollTrinkets = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "lockpicks"
        2 -> pure "a dozen poisoned needles"
        3 -> pure "embroidered cushion"
        4 -> pure "sturdy bag"
        5 -> pure "roll of bandages"
        _ -> rollBaubles
    rollBaubles = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "magic scroll"
        2 -> pure "drum"
        3 -> pure "five normal-looking eggs"
        4 -> pure "bejeweled helm"
        5 -> pure "explosive device"
        _ -> rollTreasures
    rollTreasures = do
      start <- roll (Sides 6)
      case start of
        1 -> pure "royal seal (as ring)"
        2 -> pure "priceless painting"
        3 -> pure "exotic caged bird"
        4 -> pure "deed for a major property"
        5 -> pure "impossibly rare book"
        _ -> pure "prism"
