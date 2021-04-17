module Lemming where

import Model
import Sprite 

import Movement

data Characters = Lemming 

data State = State {  coord :: Coord
                    , direction :: Deplacement 
                    , speed :: Int 
                   }
                   deriving(Show)

initState :: Int -> Int -> Int -> State
initState x y s = State (C x y) N s 

gameStep :: State -> State
gameStep st@(State c@(C x y) d s)
    | x > 0 && x < 640 = State (C(x+s) y) d s 
    |otherwise = undefined 