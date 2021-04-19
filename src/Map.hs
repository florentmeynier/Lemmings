module Map where

import Data.Map

import Movement

data Case = Vide
          | Terre
          | Metal
          | Entree
          | Sortie
    deriving (Eq, Show)

data Niveau = Niveau { hNiveau :: Int,
                       lNiveau :: Int,
                       sizeCase :: Int,
                       caseNiveau :: Map Coord Case }
    deriving (Eq, Show)

initNiveau :: Int -> Int -> Int -> Niveau
initNiveau h l s = Niveau h l s (generateMap h l)

generateMap :: Int -> Int -> Map Coord Case
generateMap h l = fromList (aux 0 0 []) where
    aux :: Int -> Int -> [(Coord, Case)] -> [(Coord, Case)]
    aux x y list
        | y == h = list
        | x == l = aux 0 (y + 1) list
        | y == 0 || x == 0 || y == h - 1 || x == l - 1 = aux (x + 1) y ((C x y, Metal):list)
        | otherwise = aux (x + 1) y ((C x y, Vide):list)

addGround :: Map Coord Case -> Map Coord Case
addGround m = fromList (aux (toList m) 0 ) where
    aux l n = if n > 10 then l else aux ((C n 10, Metal):l) (n+1) 

invNiveau :: Niveau -> Bool 
invNiveau n@(Niveau h l size m) = aux1 (toList m) 0 0 && aux2 0 0where
    aux1 :: [(Coord, Case)] -> Int -> Int -> Bool
    aux1 [] nbE nbS = nbE == 1 && nbS == 1
    aux1 ((C x y, c):q) nbE nbS
       | x == 0 || y == 0 || x == l - 1 || y == h - 1 = c == Metal && aux1 q nbE nbS
       | c == Entree && nbE == 0 = 
            case m !? bougeCoord B (C x y) of
                Just Vide -> aux1 q 1 nbS
                _ -> False
       | c == Sortie && nbS == 0 =
           case m !? bougeCoord B (C x y) of
                Just Metal -> aux1 q nbE 1
                _ -> False
       | x < 0 || y < 0 || x >= l || y >= h = False
       | otherwise = aux1 q nbE nbS
    aux2 :: Int -> Int -> Bool
    aux2 x y
        | y == h = True
        | x == l = aux2 0 (y + 1)
        | otherwise = case m !? C x y of 
            Nothing -> False 
            _ -> aux2 (x + 1) y