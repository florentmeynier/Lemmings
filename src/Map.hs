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
                               distMortel :: Int,
                               durabilite :: Int,
                               name :: [Char]
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

getInfoNiveau :: Niveau -> InfoNiveau
getInfoNiveau (Niveau _ _ _ _ _ i) = i

getDistanceMortelle :: Niveau -> Int
getDistanceMortelle n = do
    let InfoNiveau _ _ _ dist _ _ = getInfoNiveau n
    dist

getNiveauName :: Niveau -> String
getNiveauName n = do
    let InfoNiveau _ _ _ _ _ name = getInfoNiveau n
    name

getEntree :: Niveau -> Coord
getEntree (Niveau _ _ _ m _ _) = aux $ toList m where
    aux :: [(Coord, Case)] -> Coord
    aux [] = C 0 0
    aux (h:q) = 
        case h of
            (c, Entree) -> c
            _ -> aux q

initInfoNiveau :: Int -> Int -> Int -> Int -> Int -> [Char] -> InfoNiveau
initInfoNiveau = InfoNiveau

initNiveau :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Char] -> Niveau
initNiveau h l s delay max nb distMortel dur name = Niveau h l s (generateMap h l) empty (initInfoNiveau delay max nb distMortel dur name)

generateMap :: Int -> Int -> Map Coord Case
generateMap h l = fromList (aux 0 0 []) where
    aux :: Int -> Int -> [(Coord, Case)] -> [(Coord, Case)]
    aux x y list
        | y == h = list
        | x == l = aux 0 (y + 1) list
        | y == 0 || x == 0 || y == h - 1 || x == l - 1 = aux (x + 1) y ((C x y, Metal):list)
        | otherwise = aux (x + 1) y ((C x y, Vide):list)

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

prop_attaqueCase_pre :: Coord -> Niveau -> Bool
prop_attaqueCase_pre c n@(Niveau _ _ _ m _ _) = 
    case m !? c of
        Just Terre -> True
        _ -> False

attaqueCase :: Coord -> Int -> Niveau -> Niveau
attaqueCase c i n@(Niveau h l size m cass iL@(InfoNiveau _ _ _ _ dur _)) = 
    case cass !? c of
    Just x -> 
        if x <= i
        then
            Niveau h l size (insert c Vide m) (delete c cass) iL
        else
         n { cassable = insert c (x - i) cass }   
    _ -> n { cassable = insert c (dur - 1) cass }

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
            | otherwise = error ("Error wrong character " ++ c)

instance Show InfoNiveau where
    show (InfoNiveau delay max nb distMortel dur name) = show delay ++ " " ++ show max ++ " " ++ show nb ++ " " 
        ++ show distMortel ++ " " ++ show dur ++ " " ++ name ++ "\n"

instance Read InfoNiveau where
    readsPrec _ input =
        let (delay, rest1) = span isDigit input -- Récupère delai de spawn
            delay' = read delay :: Int -- Convertit delay
            (_:rest2) = rest1 -- Supprime l'espace
            (max, rest3) = span isDigit rest2 -- Récupère maxSpawn
            max' = read max :: Int-- Convertit maxSpawn
            (_:rest4) = rest3 -- Supprime l'espace
            (nb, rest5) = span isDigit rest4 -- Récupère nbExit
            nb' = read nb :: Int -- Convertit nbExit
            (_:rest6) = rest5 -- Supprime l'espace
            (distMortel, rest7) = span isDigit rest6 -- Récupère distance mortelle
            distMortel' = read distMortel :: Int -- Convertit distMortel
            (_:rest8) = rest7 -- Supprime l'espace
            (dur, rest9) = span isDigit rest8 -- Récupère durabilite
            dur' = read dur :: Int -- Convertit dur
            (_:rest10) = rest9 -- Supprime l'espace
            (name, rest11) = aux "" rest10 -- Récupère le nom du niveau
            in [(InfoNiveau delay' max' nb' distMortel' dur' (reverse name), [])]
            where
            aux :: [Char] -> [Char] -> ([Char], [Char])
            aux txt (c:q)
                | c == ' ' || c == '\n' = (txt, q)
                | otherwise = aux (c:txt) q

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
            (nb, rest11) = span isDigit rest10 -- Récupère nbExit
            nb' = read nb :: Int -- Convertit nbExit
            (_:rest12) = rest11 -- Supprime l'espace
            (distMortel, rest13) = span isDigit rest12 -- Récupère distance mortel
            distMortel' = read distMortel :: Int -- Convertit distMortel
            (_:rest14) = rest13 -- Supprime l'espace
            (dur, rest15) = span isDigit rest14 -- Récupère durabilite
            dur' = read dur :: Int -- Convertit dur
            (_:rest16) = rest15 -- Supprime l'espace
            (name, rest17) = aux1 "" rest16 -- Récupère le nom du niveau
            in [(Niveau h' l' size' (aux2 rest17 0 0 l') empty (InfoNiveau delay' max' nb' distMortel' dur' (reverse name)), [])]
            where
            aux1 :: [Char] -> [Char] -> ([Char], [Char])
            aux1 txt [] = (txt, [])
            aux1 txt (c:q)
                | c == ' ' || c == '\n' = (txt, q)
                | otherwise = aux1 (c:txt) q
            aux2 :: [Char] -> Int -> Int -> Int -> Map Coord Case
            aux2 [] x y h = empty
            aux2 (c:q) x y l
                | x == l = aux2 q 0 (y + 1) l
                | c == '\n' = aux2 q 0 (y + 1) l
                | otherwise = insert (C x y) (read [c] :: Case) (aux2 q (x + 1) y l)