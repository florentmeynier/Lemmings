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

data InfoNiveau = InfoNiveau { spawnDelay :: Int,
                               maxSpawn :: Int,
                               nbExit :: Int,
                               durabilite :: Int
                             }
    deriving (Eq)

data Niveau = Niveau { hNiveau :: Int,
                       lNiveau :: Int,
                       sizeCase :: Int,
                       caseNiveau :: Map Coord Case,
                       cassable :: Map Coord Int,
                       infoN :: InfoNiveau
                     }
    deriving (Eq)

getHauteur :: Niveau -> Int
getHauteur (Niveau h _ _ _ _ _) = h

getSize :: Niveau -> Int
getSize (Niveau _ _ size _ _ _) = size

getMap :: Niveau -> Map Coord Case
getMap (Niveau _ _ _ m _ _) = m

getCassable :: Niveau -> Map Coord Int
getCassable (Niveau _ _ _ _ cass _) = cass

getEntree :: Niveau -> Coord
getEntree (Niveau _ _ _ m _ _) = aux $ toList m where
    aux :: [(Coord, Case)] -> Coord
    aux [] = C 0 0
    aux (h:q) = 
        case h of
            (c, Entree) -> c
            _ -> aux q


initInfoNiveau :: Int -> Int -> Int -> Int -> InfoNiveau
initInfoNiveau = InfoNiveau

initNiveau :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Niveau
initNiveau h l s delay max nb dur = Niveau h l s (addEntreeSortie (generateMap h l)) empty (initInfoNiveau delay max nb dur)

generateMap :: Int -> Int -> Map Coord Case
generateMap h l = fromList (aux 0 0 []) where
    aux :: Int -> Int -> [(Coord, Case)] -> [(Coord, Case)]
    aux x y list
        | y == h = list
        | x == l = aux 0 (y + 1) list
        | y == 0 || x == 0 || y == h - 1 || x == l - 1 = aux (x + 1) y ((C x y, Metal):list)
        | otherwise = aux (x + 1) y ((C x y, Vide):list)

addEntreeSortie :: Map Coord Case -> Map Coord Case
addEntreeSortie m = insert (C 4 15) Terre (insert (C 2 15) Terre (insert (C 5 14) Terre (insert (C 1 21) Terre (insert (C 2 22) Terre (insert (C 4 22) Terre
    (insert (C 5 21) Terre (insert (C 6 20) Terre (insert (C 6 21) Terre
    (insert (C 3 15) Terre (insert (C 1 1) Entree (insert (C 3 22) Sortie m)))))))))))

addGround :: Map Coord Case -> Map Coord Case
addGround m = fromList (aux (toList m) 0 ) where
    aux l n = if n > 10 then l else aux ((C n 10, Metal):l) (n+1) 

invNiveau :: Niveau -> Bool 
invNiveau n@(Niveau h l size m _ _) = aux1 (toList m) 0 0 && aux2 0 0 where
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

attaqueCase :: Coord -> Int -> Niveau -> Niveau
attaqueCase c i n@(Niveau h l size m cass iL) = 
    case cass !? c of
    Just x -> 
        if x <= i
        then
            Niveau h l size (delete c m) (delete c cass) iL
        else
         n { cassable = insert c (x - i) cass }   
    _ -> n { cassable = insert c 20 cass }

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

instance Show InfoNiveau where
    show (InfoNiveau delay max nb dur) = show delay ++ " " ++ show max ++ " " ++ show nb ++ "\n"

instance Read InfoNiveau where
    readsPrec _ input =
        let (delay, rest1) = span isDigit input -- Récupère delai de spawn
            delay' = read delay :: Int -- Convertit delay
            (_:rest2) = rest1 -- Supprime l'espace
            (max, rest3) = span isDigit rest2 -- Récupère maxSpawn
            max' = read max :: Int-- Convertit maxSpawn
            (_:rest4) = rest3 -- Supprime l'espace
            (nb, rest5) = span isDigit rest5 -- Récupère nbExit
            nb' = read nb :: Int -- Convertit nbExit
            (_:rest6) = rest5 -- Supprime l'espace
            (dur, rest7) = span isDigit rest6 -- Récupère durabilite
            dur' = read dur :: Int -- Convertit dur
            (_:rest8) = rest7 -- Supprime le retour à la ligne
            in [(InfoNiveau delay' max' nb' dur', [])]

instance Show Niveau where
    show (Niveau h l size m _ i) = show h ++ " " ++ show l ++ " " ++ show size ++ "\n" ++ show i ++ aux 0 0 where
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
            (delay, rest7) = span isDigit rest6 -- Récupère delai de spawn
            delay' = read delay :: Int -- Convertit delay
            (_:rest8) = rest7 -- Supprime l'espace
            (max, rest9) = span isDigit rest8 -- Récupère maxSpawn
            max' = read max :: Int-- Convertit maxSpawn
            (_:rest10) = rest9 -- Supprime l'espace
            (nb,rest11) = span isDigit rest10 -- Récupère nbExit
            nb' = read nb :: Int -- Convertit nbExit
            (_:rest12) = rest11 -- Supprime l'espace
            (dur, rest13) = span isDigit rest12 -- Récupère durabilite
            dur' = read dur :: Int -- Convertit dur
            (_:rest14) = rest13 -- Supprime le retour à la ligne
            in [(Niveau h' l' size' (aux rest14 0 0 l') empty (InfoNiveau delay' max' nb' dur'), [])] where
            aux [] x y h = empty
            aux (c:q) x y l
                | x == l = aux q 0 (y + 1) l
                | c == '\n' = aux q 0 (y + 1) l
                | otherwise = insert (C x y) (read [c] :: Case) (aux q (x + 1) y l)

-- >>> show $ initNiveau 24 10 20
-- >>> read $ show $ initNiveau 24 10 20 :: Niveau



