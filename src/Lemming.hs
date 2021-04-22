module Lemming where

import Movement

data Character = Lemming Status | Creuseur Status | Mineur Status
    deriving (Eq, Show)

data Status = Marcheur State | Tombeur State Int Character | Mort State
    deriving (Eq, Show)

data State = State { coord :: Coord ,
                     direction :: Deplacement  ,
                     speed :: Int 
                    }
                    deriving(Eq, Show)

initState :: Int -> Int -> Deplacement -> Int -> State
initState x y d s = State (C x y) d s

initChar = Lemming (Marcheur (initState 30 30 D 2))
initChar2 = Tombeur (initState 5 4 B 1) 0 initChar

getStatus :: Character -> Status
getStatus (Lemming st) = st
getStatus (Creuseur st) = st
getStatus (Mineur st) = st

getState :: Character -> State
getState c = getStateFromStatus $ getStatus c

getCoordX :: Character -> Int
getCoordX c = getX (getCoordonnee c)

getCoordY :: Character -> Int
getCoordY c = getY (getCoordonnee c)

getCoordonnee :: Character -> Coord
getCoordonnee ch = do
    let State c _ _ = getState ch
    c

getDirection :: Character -> Deplacement
getDirection ch = do
    let State _ d _ = getState ch
    d

getSpeed :: Character -> Int
getSpeed ch = do
    let State _ _ s = getState ch
    s

getStateFromStatus :: Status -> State
getStateFromStatus (Marcheur st) = st
getStateFromStatus (Tombeur st _ _) = st
getStateFromStatus (Mort st) = st

getSpeedFromStatus :: Status -> Int
getSpeedFromStatus st = do
    let State _ _ s = getStateFromStatus st
    s