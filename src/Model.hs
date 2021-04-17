
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , virusX :: Int
                           , virusY :: Int
                           , speed :: Int }
  deriving (Show)


initGameState :: Int -> Int -> GameState
initGameState vx vy = GameState 200 300 vx vy 4

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ _ _ sp)  
  | px > 0 = gs { persoX = px - sp }
  |otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ _ _ sp)
  | px < 640 = gs { persoX = px + sp }
  | otherwise = gs
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py _ _ sp)
  | py > 0 = gs { persoY = py - sp }
  |otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py _ _ sp)
  | py < 480 = gs { persoY = py + sp }
  |otherwise = gs

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)

  in modif gstate
