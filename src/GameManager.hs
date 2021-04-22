module GameManager where 

import Data.Map as DMap

import Map

import Lemming

import Movement

data Game = Game { niveau :: Niveau,
                   lemmings :: [Character]
                 }
    deriving (Eq, Show)


iNiv = initNiveau 24 8 20

iMap = do
    let Niveau _ _ _ m = iNiv
    m

initGameManager :: Int -> Int -> Int -> Int -> Int -> Deplacement -> Int -> Game
initGameManager h l size x y d s = Game (initNiveau h l size) [initChar]

getFirstChar :: Game -> Character
getFirstChar (Game n (h:q)) = h 

gameStepGM :: Game -> Game
gameStepGM g@(Game n@(Niveau h l size m) lemm) = Game n (gameStepL g)

gameStepL :: Game -> [Character]
gameStepL g@(Game n@(Niveau h l size m) list) = aux1 list where
    aux1 :: [Character] -> [Character]
    aux1 [] = []
    aux1 (l:q) = tourCharacter l n:aux1 q

tourCharacter :: Character -> Niveau -> Character
tourCharacter c n = 
    case c of
    Lemming st -> tourLemming c n (getSpeed c)
    --Marcheur st -> tourMarcheur c m size (getSpeed l)
    --Tombeur st d c -> tourTombeur c m size (getSpeed l)
    --Mort st -> Mort st

tourLemming :: Character -> Niveau -> Int -> Character
tourLemming l n 0 = l
tourLemming l@(Lemming st) n tour = tourLemming (Lemming $ tourStatus st n) n (tour - 1)

tourStatus :: Status -> Niveau -> Status
tourStatus st@(Marcheur _) n = tourMarcheur st n (getSpeedFromStatus st)
tourStatus st@Tombeur {} n = tourTombeur st n (getSpeedFromStatus st)
tourStatus st@(Mort _) n = st 


tourMarcheur :: Status -> Niveau -> Int -> Status
tourMarcheur march@(Marcheur st@(State c d s)) (Niveau _ _ size m) n
    | n == 0 = march -- Tour fini
    | not $ fst $ hasGround c d m size = -- not (estDure (modifyCoord (bougeCoord B (persoCoordToMapCoord c size)) d) m) = -- Vide sous ses pieds
        tourTombeur (Tombeur (State (snd $ hasGround c d m size) B 4) 0 march) m size (n - 1) 
    | not $ estDure (modifyCoord (persoCoordToMapCoord (bougeCoord d c) size) d) m = -- Vide là où il regarde
        tourMarcheur (Marcheur st { coord = bougeCoord d c }) m size (n - 1)
    | estDure (modifyCoord (persoCoordToMapCoord (bougeCoord d c) size) d) m = -- Dure là où il regarde
        if d == G -- Regarde à gauche
        then
            if estDure (persoCoordToMapCoord (bougeCoord GH c) size) m -- Dure à gauche
            then
                tourMarcheur (Marcheur st { direction = D }) m size (n - 1) -- Se retourne à droite
            else
                tourMarcheur (Marcheur st { coord = bougeCoord G (jump c d) }) m size (n - 1) -- Avance en Haut à Gauche
        else -- Regarde à droite
            if estDure (modifyCoord (persoCoordToMapCoord (bougeCoord DH c ) size) D) m -- Dure à droite
            then
                tourMarcheur (Marcheur st { direction = G }) m size (n - 1) -- Se retourne à Gauche
            else
                tourMarcheur (Marcheur st { coord = bougeCoord D (jump c d) }) m size (n - 1) -- Avance en Haut à Droite
    | otherwise = tourMarcheur march m size (n - 1)

tourTombeur :: Status -> Niveau -> Int -> Status
tourTombeur tomb@(Tombeur (State c d s) dist char) (Niveau _ _ size m) n 
    | n == 0 = tomb
    | fst $ hasGround c d m size =  -- estDure (modifyCoord (bougeCoord B (persoCoordToMapCoord c size)) (getDirection char)) m = --hasGround (persoCoordToMapCoord c size) m size = 
        if dist `div` size > 50 -- Vérifie si la distance est mortelle
        then 
            Mort (State c d s) -- Mort
        else 
            tourMarcheur (Marcheur (State c (getDirection char) (getSpeed char))) m size (n - 1) -- Devient Marcheur
    | otherwise = -- Continue de tomber
        tourTombeur (Tombeur (State (bougeCoord B c) d s) (dist + 1) char) m size (n - 1)

jump :: Coord -> Deplacement  -> Coord 
jump (C x y) D = C (x + 1) (y - 20)
jump (C x y) G = C x (y - 20)

modifyCoord :: Coord -> Deplacement -> Coord 
modifyCoord (C x y) d
    | d == D = C (x + 1) y
    | otherwise = C x y

hasGround :: Coord -> Deplacement -> Map Coord Case -> Int -> (Bool, Coord)
hasGround c@(C x y) d m size 
    | d == D && x `mod` size == 19 = -- 
        (estDure (bougeCoord DB (persoCoordToMapCoord c size)) m, C (x + 1) y)
    | x `mod` size == 0 = 
        (estDure (bougeCoord B (persoCoordToMapCoord c size)) m, c)
    | otherwise = (estDure (bougeCoord B (persoCoordToMapCoord c size)) m || -- Sol sous ses pieds OU
        not (estDure (bougeCoord B (persoCoordToMapCoord c size)) m) && -- Vide sous ses pieds
        estDure (bougeCoord DB (persoCoordToMapCoord c size)) m, c) -- ET sol 
