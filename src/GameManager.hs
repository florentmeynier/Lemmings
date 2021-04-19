module GameManager where 

import Data.Map as DMap

import Map

import Lemming

import Movement

data Game = Game { niveau :: Niveau,
                   lemming :: State }
    deriving (Eq, Show)

initGameManager :: Int -> Int -> Int -> Int -> Int -> Deplacement -> Int -> Game
initGameManager h l size x y d s = Game (initNiveau h l size) (initState x y d s)

gameStepGM :: Game -> Game
gameStepGM g@(Game n@(Niveau h l size m) lemm) = Game n (gameStepL g)

gameStepL :: Game -> State
gameStepL g@(Game n@(Niveau h l size m) st@(State c@(C x y) d s)) = aux st s where
    aux :: State -> Int -> State
    aux st@(State c@(C x y) d s) n
        | n == 0 =st
        | not (hasGround c m size) = aux (State (bougeCoord B c) B s) (n - 1)  
        | hasGround c m size && d == B = aux (State c G s) (n - 1)
        | n > 0 && canMove st m h l size = aux (State (bougeCoord d c) d s) (n - 1)
        | n > 0 && d == G = aux (State c D s) (n - 1) 
        | n > 0 && d == D = aux (State c G s) (n - 1)
        | otherwise = st

canMove :: State -> Map Coord Case -> Int -> Int -> Int -> Bool 
canMove st@(State (C x y) d _) m h l size = 
    let x = modifyCoord st size in
    let c'@(C x' y') = bougeCoord d (C (x `div` size) (y `div` size)) in
    x' > 0 && y' > 0 && x' < l && y' < l &&
    case m !? c' of
        Just Metal -> False 
        _ -> True

modifyCoord :: State -> Int -> Int
modifyCoord st@(State (C x y) d s) size
    | d == G = x + size
    | otherwise = x

hasGround :: Coord -> Map Coord Case -> Int-> Bool
hasGround (C x y) m size = 
    let c'@(C x' y') = bougeCoord B (C (x `div` size) (y `div` size)) in
    case m !? c' of
    Just Vide -> False
    Nothing -> False
    _ -> True