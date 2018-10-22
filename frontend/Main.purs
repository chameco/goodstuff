module Main where

import Prelude
import Saturnal.Event
import Saturnal.Net
import Saturnal.Render
import Saturnal.State
import Saturnal.Types
import Saturnal.UI

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, log, logShow)
import Foreign.Generic (genericDecodeJSON)
import Graphics.Canvas (CanvasElement, Context2D, clearRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, restore, save, setCanvasHeight, setCanvasWidth, translate)
import Web.HTML (window)
import Web.HTML.Window (alert, outerHeight, outerWidth)

boundCamera :: State -> State
boundCamera (State v m board) = State (v { x = clamp 0.0 (3.0 * v.r * case board of Board b -> toNumber b.boardWidth) v.x
                                         , y = clamp 0.0 (v.r * case board of Board b -> toNumber b.boardHeight) v.y
                                         }) m board

updateBoard :: Effect Unit
updateBoard = do
  game <- getGame
  case game of
    Just g -> do
      state <- getState
      let turn = case state of Just (State v m (Board b)) -> Just b.boardTurn
                               Nothing -> Nothing
      poll g $ \boardJSON -> do
        log boardJSON
        case runExcept $ genericDecodeJSON opts boardJSON of
          Right (Board board) -> do
            if Just board.boardTurn == turn
              then after 5000.0 updateBoard
              else do case state of Just (State v m _) -> setState (State v [] (Board board))
                                    Nothing -> setState (State { r: 50.0, x: 0.0, y: 0.0 } [] (Board board))
                      unhide "canvas"
          _ -> error "Server sent invalid response"
    Nothing -> window >>= alert "Join a game first"

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
      listen "hex" "click" $ toggle "menu"
      listen "endturn" "click" $ do
        hide "canvas"
        updateBoard
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
      listen "alpha" "click" $ do
        popup "alpha" "Resource α" "<i>placeholder</i>"
      listen "beta" "click" $ do
        popup "beta" "Resource β" "placeholder"
      listen "gamma" "click" $ do
        popup "gamma" "Resource γ" "placeholder"
      listen "delta" "click" $ do
        popup "delta" "Resource δ" "placeholder"
      frames $ do
        state <- getState
        case state of
          Just (State v m b) -> do
            width <- getCanvasWidth canvas
            height <- getCanvasHeight canvas
            clearRect ctx {x: 0.0, y: 0.0, width: width, height: height}
            save ctx
            translate ctx { translateX: (width / 2.0) - v.x, translateY: (height / 2.0) - v.y}
            renderBoard ctx v.r b
            restore ctx
          Nothing -> pure unit
