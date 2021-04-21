module Map where

import Data.Map

import Movement

import Data.Char (isDigit)

data Case = Vide
          | Terre
          | Metal
          | Entree
          | Sortie
    deriving (Eq)

data Niveau = Niveau { hNiveau :: Int,
                       lNiveau :: Int,
                       sizeCase :: Int,
                       caseNiveau :: Map Coord Case
                     }
    deriving (Eq)

initNiveau :: Int -> Int -> Int -> Niveau
initNiveau h l s = Niveau h l s (addEntreeSortie (generateMap h l))

generateMap :: Int -> Int -> Map Coord Case
generateMap h l = fromList (aux 0 0 []) where
    aux :: Int -> Int -> [(Coord, Case)] -> [(Coord, Case)]
    aux x y list
        | y == h = list
        | x == l = aux 0 (y + 1) list
        | y == 0 || x == 0 || y == h - 1 || x == l - 1 = aux (x + 1) y ((C x y, Metal):list)
        | otherwise = aux (x + 1) y ((C x y, Vide):list)

addEntreeSortie :: Map Coord Case -> Map Coord Case
addEntreeSortie m = insert (C 3 22) Terre (insert (C 1 1) Entree (insert (C 3 2) Sortie m))

addGround :: Map Coord Case -> Map Coord Case
addGround m = fromList (aux (toList m) 0 ) where
    aux l n = if n > 10 then l else aux ((C n 10, Metal):l) (n+1) 

invNiveau :: Niveau -> Bool 
invNiveau n@(Niveau h l size m) = aux1 (toList m) 0 0 && aux2 0 0 where
    aux1 :: [(Coord, Case)] -> Int -> Int -> Bool -- Test 1 entree && 1 sortie && Coordonnées entre 0 et h/l 
    aux1 [] nbE nbS = nbE == 1 && nbS == 1
    aux1 ((C x y, c):q) nbE nbS
        | x == 0 || y == 0 || x == l - 1 || y == h - 1 = c == Metal && aux1 q nbE nbS -- Contour Metal
        | c == Entree && nbE == 0 =
            case m !? bougeCoord B (C x y) of
                Just Vide -> aux1 q 1 nbS
                _ -> False
        | c == Sortie && nbS == 0 =
           case m !? bougeCoord B (C x y) of
                Just Metal -> aux1 q nbE 1
                _ ->  False
        | x < 0 || y < 0 || x >= l || y >= h = False
        | otherwise = aux1 q nbE nbS
    aux2 :: Int -> Int -> Bool -- Test si toutes les cases existent
    aux2 x y
        | y == h = True
        | x == l = aux2 0 (y + 1)
        | otherwise = case m !? C x y of 
            Nothing -> False
            _ -> aux2 (x + 1) y

estDure :: Coord -> Map Coord Case -> Bool
estDure c m = 
    case m !? c of
        Just Metal -> True
        Just Terre -> True 
        _ -> False

instance Show Case where
    show c = 
        case c of
            Vide -> " "
            Terre -> "0"
            Metal -> "X"
            Entree -> "E"
            Sortie -> "S"
        
instance Read Case where
    readsPrec _ input =
        let [h] = input in
        [(aux [h], "")] where
        aux c
            | c == " " = Vide
            | c == "0" = Terre
            | c == "X" = Metal
            | c == "E" = Entree
            | c == "S" = Sortie
            | otherwise = error "Error"

instance Show Niveau where
    show n@(Niveau h l size m) = show h ++ " " ++ show l ++ " " ++ show size ++ "\n" ++ aux 0 0 where
        aux :: Int -> Int -> String
        aux x y
            | y == h = ""
            | x == l = "\n" ++ aux 0 (y + 1)
            | otherwise = maybe "" show (m !? C x y) ++ aux (x + 1) y

instance Read Niveau where
    readsPrec _ input = 
        let (h, rest1) = span isDigit input -- Récupère h
            h' = read h :: Int -- Convertit h
            (_:rest2) = rest1 -- Supprime l'espace
            (l, rest3) = span isDigit rest2 -- Récupère l
            l' = read l :: Int -- Convertit l
            (_:rest4) = rest3 -- Supprime l'espace
            (size, rest5) = span isDigit rest4 -- Récupère size
            size' = read size :: Int -- Convertit size
            (_:rest6) = rest5 -- Supprime le retour à la ligne
            in [(Niveau h' l' size' (aux rest6 0 0 l'), [])] where
            aux [] x y h = empty
            aux (c:q) x y l
                | x == l = aux q 0 (y + 1) l
                | c == '\n' = aux q 0 (y + 1) l
                | otherwise = insert (C x y) (read [c] :: Case) (aux q (x + 1) y l)