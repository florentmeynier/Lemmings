module Lemming where

import Movement

data Characters = Marcheur State | Tombeur State Int Characters | Mort State
    deriving (Eq, Show)

data State = State { coord :: Coord ,
                     direction :: Deplacement  ,
                     speed :: Int 
                    }
                    deriving(Eq, Show)

initState :: Int -> Int -> Deplacement -> Int -> State
initState x y d s = State (C x y) d s

initChar = Marcheur (initState 30 30 G 2)
initChar2 = Tombeur (initState 5 4 B 1) 0 initChar

getState :: Characters -> State
getState (Marcheur st) = st
getState (Tombeur st _ _) = st
getState (Mort st) = st

getCoordX :: Characters -> Int
getCoordX c = getX (getCoordonnee c)

getCoordY :: Characters -> Int
getCoordY c = getY (getCoordonnee c)

getCoordonnee :: Characters -> Coord
getCoordonnee ch = do
    let State c _ _ = getState ch
    c

getDirection :: Characters -> Deplacement
getDirection ch = do
    let State _ d _ = getState ch
    d

getSpeed :: Characters -> Int
getSpeed ch = do
    let State _ _ s = getState ch
    s