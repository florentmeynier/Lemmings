module Lemming where

import Movement

data Characters = Lemming 

data State = State {  coord :: Coord
                    , direction :: Deplacement 
                    , speed :: Int 
                   }
                   deriving(Eq, Show)

initState :: Int -> Int -> Deplacement -> Int  -> State
initState x y d s = State (C x y) d s 

gameStepL2 :: State -> State
gameStepL2 st@(State c@(C x y) d s) = aux st s where
    aux :: State -> Int -> State
    aux st@(State c@(C x y) d s) n
        | n > 0 && canMove2 st = aux (State (bougeCoord d c) d s) (n - 1)
        | n > 0 && d == G = aux (State c D s) (n - 1) 
        | n > 0 && d == D = aux (State c G s) (n - 1)
        | otherwise = st

canMove2 :: State -> Bool 
canMove2 st@(State c@(C x y) d s)
    | d == G && x > 0 = True
    | d == D && x < 640 = True
    | d == H && y > 0 = True
    | d == B && y < 480 = True 
    | d == GH && x > 0 && y > 0 = True
    | d == GB && x > 0 && y < 480 = True 
    | d == DH && x < 640 && y > 0 = True
    | d == DB && x < 640 && y < 480 = True 
    | otherwise = False
