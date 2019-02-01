module Saturnal.Main where

import Control.Bind (bind, discard, pure, (>>=))
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<), (>>>))
import Data.Array (fold, intercalate)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Field ((/))
import Data.Function (($))
import Data.Functor (map)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Ord ((<=))
import Data.Ring (negate, (-))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (error)
import Foreign.Generic (genericDecodeJSON, genericEncodeJSON)
import Graphics.Canvas (clearRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, restore, save, setCanvasHeight, setCanvasWidth, translate)
import Saturnal.Describe (describeCell, listenDescribeCell)
import Saturnal.Event (after, frames, key, listen, mousedown, resize)
import Saturnal.Net (getGame, getPlayer, invite, login, newGame, poll, setGame, submitTurn)
import Saturnal.Render (hexToAbsolute, nearestHex, outlineHex, renderBoard, renderMove, viewportToAbsolute)
import Saturnal.State (State(..), boundCamera, cellAt, getState, setState)
import Saturnal.Types (Board(..), Player(..), Turn(..), opts)
import Saturnal.UI (description, display, getValue, hide, popup, setHTML, toggle, undisplay, unhide)
import Web.HTML (window)
import Web.HTML.Window (alert, outerHeight, outerWidth)

fetchBoard :: (Board -> Effect Unit) -> Effect Unit
fetchBoard handler = do
  game <- getGame
  case game of
    Just g -> do
      poll g $ \boardJSON -> do
        case runExcept $ genericDecodeJSON opts boardJSON of
          Right board -> handler board
          _ -> error "Server sent invalid response"
    Nothing -> window >>= alert "Join a game first"

updateBoard :: Effect Unit
updateBoard = fetchBoard $ \(Board board) -> do
  state <- getState
  let turn = case state of Just (State v m (Board b)) -> b.boardTurn
                           Nothing -> -1
  if board.boardTurn <= turn
    then after 5000.0 updateBoard
    else do case state of Just (State v m _) -> setState (State v [] (Board board))
                          Nothing -> setState (State { r: 50.0, x: 0.0, y: 0.0, primary: Nothing, secondary: Nothing } [] (Board board))
            refreshPlayers
            unhide "canvas"

refreshPlayers :: Effect Unit
refreshPlayers = do
  state <- getState
  case state of
    Just (State _ _ (Board b)) -> setHTML "joinedgameplayers" $ intercalate ", " $ map (\(Player p) -> p.playerName) $ b.boardPlayers
    Nothing -> pure unit

main :: Effect Unit
main = do
  getCanvasElementById "canvas" >>= \c -> case c of
    Nothing -> error "Canvas element not found"
    Just canvas -> do
      win <- window
      outerWidth win >>= toNumber >>> setCanvasWidth canvas
      outerHeight win >>= toNumber >>> setCanvasHeight canvas
      ctx <- getContext2D canvas
      resize $ do
        outerWidth win >>= toNumber >>> setCanvasWidth canvas
        outerHeight win >>= toNumber >>> setCanvasHeight canvas
      key "ArrowLeft" $ do
        state <- getState
        case state of
          Just (State v m b) -> setState <<< boundCamera $ State (v { x = v.x - 10.0 }) m b
          Nothing -> pure unit
      key "ArrowRight" $ do
        state <- getState
        case state of
          Just (State v m b) -> setState <<< boundCamera $ State (v { x = v.x + 10.0 }) m b
          Nothing -> pure unit
      key "ArrowUp" $ do
        state <- getState
        case state of
          Just (State v m b) -> setState <<< boundCamera $ State (v { y = v.y - 10.0 }) m b
          Nothing -> pure unit
      key "ArrowDown" $ do
        state <- getState
        case state of
          Just (State v m b) -> setState <<< boundCamera $ State (v { y = v.y + 10.0 }) m b
          Nothing -> pure unit
      mousedown "canvas" $ \button -> \x -> \y -> do
        state <- getState
        width <- getCanvasWidth canvas
        height <- getCanvasHeight canvas
        case state of
          Just (State v m b) -> do
            let selection = nearestHex v b (Tuple width height) (Tuple x y)
            if button == 0.0
              then do
                setState $ State (v {primary = selection }) m b
                case selection of
                  Just (Tuple hx hy) ->
                    case cellAt b (Tuple hx hy) of
                      Nothing -> pure unit
                      Just cell -> do
                        description "" "" ""
                        description "" "" ""
                        popup (show selection) (fold ["Cell at (", show hx, ",", show hy, ")"]) $ describeCell cell
                        listenDescribeCell cell
                  Nothing -> do
                    description "" "" ""
                    description "" "" ""
                    popup "" "" ""
                    popup "" "" ""
              else setState $ State (v {secondary = selection }) m b
          Nothing -> pure unit
      listen "hex" "click" $ toggle "menu"
      listen "endturn" "click" $ do
        state <- getState
        player <- getPlayer
        case Tuple state player of
          Tuple (Just (State v m b)) (Just p) -> do
            hide "canvas"
            description "" "" ""
            description "" "" ""
            popup "" "" ""
            popup "" "" ""
            submitTurn (genericEncodeJSON opts $ Turn { turnPlayer: p, turnMoves: m, turnBid: 0 }) updateBoard
          _ -> pure unit
      listen "login" "click" $ do
        user <- getValue "username"
        pass <- getValue "password"
        case Tuple user pass of
          Tuple (Just u) (Just p) -> login u p $ do
            setHTML "loggedinas" u
            undisplay "loginmenu"
            display "loggedinmenu"
          _ -> pure unit
      listen "joingame" "click" $ do
        gameid <- getValue "gameid"
        case gameid of
          Just g -> do
            setGame g
            setHTML "joinedgame" g
            undisplay "joingamemenu"
            display "joinedgamemenu"
            hide "menu"
            updateBoard
          Nothing -> pure unit
      listen "newgame" "click" $ do
        width <- getValue "newgamewidth"
        height <- getValue "newgameheight"
        case Tuple width height of
          Tuple (Just w) (Just h) -> newGame w h $ \g -> do
            setGame g
            setHTML "joinedgame" g
            undisplay "joingamemenu"
            display "joinedgamemenu"
            hide "menu"
            updateBoard
          _ -> pure unit
      listen "invite" "click" $ do
        player <- getValue "inviteplayer"
        case player of
          Just p -> invite p $ do
            after 1000.0 <<< fetchBoard $ \(Board board) -> do
              state <- getState
              case state of
                Just (State v m _) -> do
                  setState <<< State v m $ Board board
                  refreshPlayers
                _ -> pure unit
          Nothing -> pure unit
      listen "alpha" "click" $ do
        popup "alpha"
          "Resource α"
          "<i>Spent to play cards</i><hr>"
      listen "beta" "click" $ do
        popup "beta"
          "Resource β"
          "<i>Spent to draw cards</i><hr>"
      listen "gamma" "click" $ do
        popup "gamma"
          "Resource γ"
          "<i>Common currency</i><hr>"
      listen "delta" "click" $ do
        popup "delta"
          "Resource δ"
          "<i>Bid for turn priority</i><hr>"
      frames $ do
        state <- getState
        case state of
          Just (State v m b) -> do
            width <- getCanvasWidth canvas
            height <- getCanvasHeight canvas
            clearRect ctx {x: 0.0, y: 0.0, width: width, height: height}
            save ctx
            translate ctx { translateX: (width / 2.0) - v.x, translateY: (height / 2.0) - v.y}
            renderBoard ctx (viewportToAbsolute v (Tuple width height) $ Tuple 0.0 0.0) (viewportToAbsolute v (Tuple width height) $ Tuple width height) v.r b
            case v.primary of
              Just (Tuple x y) -> do
                let Tuple ax ay = hexToAbsolute v (Tuple x y)
                outlineHex ctx "#00ff00" v.r ax ay
              Nothing -> pure unit
            case v.secondary of
              Just (Tuple x y) -> do
                let Tuple ax ay = hexToAbsolute v (Tuple x y)
                outlineHex ctx "#ff0000" v.r ax ay
              Nothing -> pure unit
            for_ m $ renderMove ctx v b
            restore ctx
          Nothing -> pure unit
