module GameManager where 

import Data.Map as DMap

import Map

import Lemming

import Movement

data Game = Game { niveau :: Niveau,
                   lemmings :: [Characters]
                 }
    deriving (Eq, Show)

initGameManager :: Int -> Int -> Int -> Int -> Int -> Deplacement -> Int -> Game
initGameManager h l size x y d s = Game (initNiveau h l size) [initChar]

getFirstChar :: Game -> Characters
getFirstChar (Game n (h:q)) = h 

gameStepGM :: Game -> Game
gameStepGM g@(Game n@(Niveau h l size m) lemm) = Game n (gameStepL g)

gameStepL :: Game -> [Characters]
gameStepL g@(Game n@(Niveau h l size m) list) = aux1 list where
    aux1 :: [Characters] -> [Characters]
    aux1 [] = []
    aux1 (l:q) = tourLemming l m size:aux1 q

tourLemming :: Characters -> Map Coord Case -> Int -> Characters
tourLemming l m size = 
    case l of
    Marcheur st -> tourMarcheur l m size (getSpeed l)
    Tombeur st d c -> tourTombeur l m size (getSpeed l)
    Mort st -> Mort st

gameStepLoop g 0 = g
gameStepLoop g n = gameStepLoop (gameStepGM g) (n - 1)

-- >>> gameStepLoop (initGameManager 24 32 20 20 50 D 1) 5

iNiv = initNiveau 24 32 20

iMap = do
    let Niveau _ _ _ m = iNiv
    m

iMarcheur = Marcheur (State (C 40 440) G 1)

-- 3 22 -> 40 440 [60 - 80]

-- >>> persoCoordToMapCoord (C 81 440) 20
-- C 4 22

-- >>> tourMarcheur iMarcheur iMap 20 1
-- >>> estDure (persoCoordToMapCoord (bougeCoord G (C 80 440)) 20) iMap
-- >>> estDure (bougeCoord D (persoCoordToMapCoord (C 58 440) 20)) iMap
-- >>> estDure (modifyCoord (bougeCoord G (persoCoordToMapCoord (C 40 440) 20)) G False 20) iMap
-- Marcheur (State {coord = C 39 440, direction = G, speed = 1})
-- False
-- True
-- False

-- >>> mapCoordToPersoCoord (C 15 22) 20
-- C 300 440

-- >>> getCoordonnee (tourMarcheur (March)

-- >>> estDure (C 15 22) iMap
-- >>> estDure (bougeCoord B (C 14 21)) iMap
-- True
-- False

tourMarcheur :: Characters -> Map Coord Case -> Int -> Int -> Characters
tourMarcheur march@(Marcheur st@(State c d s)) m size n
    | n == 0 = march -- Tour fini
    | not (estDure (bougeCoord B (persoCoordToMapCoord c size)) m) = -- Vide sous ses pieds
        tourTombeur (Tombeur (State c B 4) 0 march) m size (n - 1) --(State lem (bougeCoord B c) B s) m size (n - 1) 
    | not (estDure (modifyCoord (bougeCoord d (persoCoordToMapCoord c size)) d False size) m) = -- Vide là où il regarde
        tourMarcheur (Marcheur st { coord = bougeCoord d c }) m size (n - 1) --(State lem (bougeCoord d c) d s) m size (n - 1)
    | estDure (modifyCoord (bougeCoord d (persoCoordToMapCoord c size)) d False size) m = -- Dure là où il regarde
        if d == G -- Regarde à gauche
        then
            if estDure (modifyCoord (bougeCoord GH (persoCoordToMapCoord c size)) d False size) m -- Dure à gauche
            then
                tourMarcheur (Marcheur st { direction = D }) m size (n - 1)
                --tourLemming (State lem c D s) m size (n - 1) -- Se retourne à droite
            else
                tourMarcheur (Marcheur st { coord = bougeCoord G (jump c) }) m size (n - 1)
                --tourLemming (State lem (bougeCoord GH c) d s) m size (n - 1) -- Avance en Haut à Gauche
        else
            if estDure (modifyCoord (bougeCoord DH (persoCoordToMapCoord c size)) d False size) m -- Dure à droite
            then
                tourMarcheur (Marcheur st { direction = G }) m size (n - 1)
                --tourLemming (State lem c G s) m size (n - 1) -- Se retourne à Gauche
            else
                tourMarcheur (Marcheur st { coord = bougeCoord DH c }) m size (n - 1)
                --tourLemming (State lem (bougeCoord DH c) d s) m size (n - 1) -- Avance en Haut à Droite
    | otherwise = tourMarcheur march m size (n - 1)

tourTombeur :: Characters -> Map Coord Case -> Int -> Int -> Characters
tourTombeur tomb@(Tombeur (State c d s) dist char) m size n 
    | n == 0 = tomb
    | estDure (bougeCoord d (persoCoordToMapCoord c size)) m = --hasGround (persoCoordToMapCoord c size) m size = 
        if dist `div` size > 50 -- Vérifie si la distance est mortelle
        then 
            Mort (State c d s) -- Mort
        else 
            tourMarcheur (Marcheur (State c (getDirection char) (getSpeed char))) m size (n - 1) -- Devient Marcheur
    | otherwise = -- Continue de tomber
        tourTombeur (Tombeur (State (bougeCoord B c) d s) (dist + 1) char) m size (n - 1)

jump :: Coord -> Coord 
jump (C x y) = C x (y - 20)

modifyCoord :: Coord -> Deplacement -> Bool -> Int -> Coord 
modifyCoord (C x y) d b size
    | d == G && b = C (x + size) y 
    | d == G = C (x + 1) y
    | otherwise = C x y

hasGround :: Coord -> Map Coord Case -> Int-> Bool
hasGround (C x y) m size = 
    let c'@(C x' y') = bougeCoord B (C (x `div` size) (y `div` size)) in
    case m !? c' of
    Just Vide -> False
    Nothing -> False
    _ -> True
