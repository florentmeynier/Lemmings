module GameManager where

import Data.Map as DMap

import Data.List as List

import Map

import Lemming

import Movement

data GameStatus = Pending | Playing | Win | Loose
    deriving (Eq, Show)

data InfoGame = InfoGame {
                            nbSpawn :: Int,
                            nbExit :: Int,
                            game_status :: GameStatus
                         }
    deriving (Eq, Show)

data Game = Game { niveau :: Niveau,
                   lemmings :: [Character],
                   infoG :: InfoGame,
                   clique :: Int
                 }
    deriving (Eq, Show)

getNiveau :: Game -> Niveau
getNiveau (Game n _ _ _) = n

getLemmings :: Game -> [Character]
getLemmings (Game _ l _ _) = l

iNiv = initNiveau 24 10 20 5 5 5 5 30 ""

iMap = do
    let Niveau _ _ _ m _ _= iNiv
    m

initInfoGame = InfoGame 0 0 Playing

initGameManager :: Int -> Int -> Int -> Deplacement -> Int -> Game
initGameManager h l size d s = Game (initNiveau h l size 5 5 5 5 30 "") [] initInfoGame 0

gameStepGameManager :: Game -> Int -> Game
gameStepGameManager g@(Game n@(Niveau _ _ _ _ _ (InfoNiveau delay max _ _ _ _)) lemm infoG@(InfoGame nbSpawn nbE gs) cl) tick =
    let (lemm', n') = gameStepLemmings lemm n in -- Fait 1 tour tous les Lemmings
    case spawnCharacter g tick of -- Vérifie si un nouveau Lemming doit apparaitre
        Just c -> Game n' (c:lemm') (InfoGame (nbSpawn + 1) (nbE + List.length lemm - List.length lemm') gs) cl -- Ajout du lemming + augmente le nombre total
        Nothing -> Game n' lemm' (InfoGame nbSpawn (nbE + List.length lemm - List.length lemm') gs) cl -- Ne fait rien 

spawnCharacter :: Game -> Int -> Maybe Character
spawnCharacter (Game n@(Niveau _ _ size _ _ (InfoNiveau delay max _ _ _ _)) lemm (InfoGame nbSpawn nbExit _) _) tick
    | tick `mod` delay == 0 && nbSpawn < max = -- Ajout d'un nouveau Lemming
        Just $ Lemming (Marcheur (State (mapCoordToPersoCoord (getEntree n) size) D 1))
    | otherwise = Nothing -- N'ajoute pas de nouveau Lemming

gameStepLemmings :: [Character] -> Niveau -> ([Character], Niveau)
gameStepLemmings [] n = ([], n)
gameStepLemmings (l:q) n =
    case tourCharacter l n of
        Just (c', n') -> let res = gameStepLemmings q n' in
                    (c':fst res, snd res)
        Nothing -> gameStepLemmings q n

tourCharacter :: Character -> Niveau -> Maybe (Character, Niveau)
tourCharacter c n = do -- Tour d'un Character
    case getMap n !? persoCoordToMapCoord (modifyCoord (getCoordonnee c) (getDirection c)) (getSize n) of
        Just Sortie -> Nothing
        _ -> case c of
            Lemming _ -> Just (tourLemming c n (getSpeed c), n) -- Si Lemming
            Flotteur _ -> Just (tourFlotteur c n (getSpeed c), n) -- Si Flotteur
            Grimpeur _ _ -> Just (tourGrimpeur c n (getSpeed c), n) -- Si Grimpeur
            Pelleteur _ -> Just $ tourPelleteur c n (getSpeed c) -- Si Pelleteur
            Creuseur _ -> Just $ tourCreuseur c n (getSpeed c) -- Si Creuseur

tourLemming :: Character -> Niveau -> Int -> Character
tourLemming l _ 0 = l -- Lemming a fini son tour
tourLemming l@(Lemming st) n tour = tourLemming (Lemming $ tourStatus st n) n (tour - 1) -- Lemming a au moins 1 tour
tourLemming l _ _ = l -- Ce n'est pas un Lemming

tourFlotteur :: Character -> Niveau -> Int -> Character
tourFlotteur f _ 0 = f -- Flotteur a fini son tour
tourFlotteur f@(Flotteur st) n tour = -- Flotteur a au moins 1 tour
    case tourStatus st n of
        Tombeur (State c d s) dist state2 -> tourFlotteur (Flotteur $ Tombeur (State c d 1) 0 state2) n (tour - 1)
        statut -> tourFlotteur (Flotteur statut) n (tour - 1)

tourGrimpeur :: Character -> Niveau -> Int -> Character
tourGrimpeur g _ 0 = g -- Grimpeur a fini sou tour
tourGrimpeur g@(Grimpeur st b) n@(Niveau _ _ size m _ _) tour = -- Grimpeur a au moins 1 tour
    case st of -- Test le statut du Grimpeur
        Marcheur state@(State c d s) -> -- Si Marcheur
            case auxGrimpeur state b of
                Just (b', c') -> tourGrimpeur (Grimpeur (Marcheur (State c' d s)) b') n (tour - 1)
                Nothing -> tourGrimpeur (Grimpeur (tourStatus st n) False) n (tour - 1)
        _ -> tourGrimpeur (Grimpeur (tourStatus st n) b) n (tour - 1)
    where
    auxGrimpeur :: State -> Bool -> Maybe (Bool, Coord)
    auxGrimpeur st@(State c@(C x y) d s) b
        | not b =
            if estDure (modifyCoord (persoCoordToMapCoord (bougeCoord H c) size) d) m
            then
                Nothing
            else
                auxGrimpeur st True
        | not (estDure (modifyCoord (persoCoordToMapCoord (bougeCoord H c) size) d) m) = -- Si vide au dessus
            if estDure (modifyCoord (persoCoordToMapCoord (bougeCoord d c) size) d) m
            then -- Si vide là où il regarde
                if d == D
                then -- Si regarde à droite
                    if estDure (modifyCoord (persoCoordToMapCoord (bougeCoord DH c) size) D) m
                    then -- Si dur en haut à droite
                        Just (b, C x (y - 1)) -- Monte
                    else
                        Just (not b, jump (bougeCoord d c) d) -- Saute au dessus
                else -- Si regarde à gauche
                    if estDure (persoCoordToMapCoord (bougeCoord GH c) size) m
                    then -- Si dur en haut à gauche
                        Just (b, C x (y - 1)) -- Monte
                    else
                        Just (not b, jump (bougeCoord d c) d) -- Saute au dessus
            else
                Nothing
        | not $ estDure (modifyCoord (persoCoordToMapCoord (bougeCoord B c) size) d) m -- Si il y a du vide sous ses pieds 
          && estDure (modifyCoord (persoCoordToMapCoord (bougeCoord d c) size) d) m = -- et qu'il y a un mur face à lui          
                Just (b, c)
        | otherwise = Nothing

tourPelleteur :: Character -> Niveau -> Int -> (Character, Niveau)
tourPelleteur p n 0 = (p, n) -- Pelleteur a fini son tour
tourPelleteur p@(Pelleteur st) n tour = -- Pelleteur a au moins 1 tour
    case st of -- Test le statut du Pelleteur
        Marcheur state -> -- Si Marcheur, doit essayer de creuser
            let (p', n') = auxPelleteur state in
            tourPelleteur p' n' (tour - 1)
        _ -> tourPelleteur (Pelleteur $ tourStatus st n) n (tour - 1) -- Sinon, ne fait rien
    where
    auxPelleteur :: State -> (Character, Niveau)
    auxPelleteur state@(State c d s) = -- Tour Pelleteur
        let check = modifyCoord (persoCoordToMapCoord (bougeCoord d c) (getSize n)) d in -- Récupére le contenu de la case qu'il regarde
        case getMap n !? check of
        (Just Terre) -> -- Si la case peut être creusée
            (p, attaqueCase check 1 n)
        _ -> -- Si la case ne peut pas être creusée
            (Pelleteur $ tourStatus st n, n)
tourPelleteur p n _ = (p, n) -- Ce n'est pas un Pelleteur

tourCreuseur :: Character -> Niveau -> Int -> (Character, Niveau)
tourCreuseur cr n 0 = (cr, n) -- Creuseur a fini son tour
tourCreuseur cr@(Creuseur st) n tour = -- Creuseur a au moins 1 tour
    case st of -- Test le statut du Creuseur
        Marcheur state -> -- Si Marcheur, doit essayer de creuser
            let (cr', n') = auxCreuseur state in
            tourCreuseur cr' n' (tour - 1)
        _ -> tourCreuseur (Creuseur $ tourStatus st n) n (tour - 1) -- Sinon, ne fait rien
    where
    auxCreuseur :: State -> (Character, Niveau)
    auxCreuseur state@(State (C x y) d s) -- Tour Creuseur
        | x `mod` getSize n == 0 =
            let check = bougeCoord B (persoCoordToMapCoord (C x y) (getSize n)) in
            case getMap n !? check of
            Just Terre -> -- Si la case peut être creusée
                (cr, attaqueCase check 1 n)
            _ -> (Creuseur $ tourStatus st n, n)
        | otherwise  = (Creuseur $ tourStatus st n, n)
tourCreuseur cr n _ = (cr, n) -- Ce n'est pas un Creuseur

tourStatus :: Status -> Niveau -> Status
tourStatus st@(Marcheur _) n = tourMarcheur st n
tourStatus st@Tombeur {} n = tourTombeur st n
tourStatus st@(Mort _) n = st

-- >>> tourLemming (Lemming (Marcheur (State (C 59 40) D 1))) iNiv 2
-- Lemming (Marcheur (State {coord = C 60 40, direction = G, speed = 1}))

-- >>> estDure (modifyCoord (persoCoordToMapCoord (bougeCoord D (C 59 40)) 20) D) (getMap iNiv)
-- True


tourMarcheur :: Status -> Niveau -> Status
tourMarcheur march@(Marcheur st@(State c d s)) n@(Niveau _ _ size m _ _)
    | not $ fst $ hasGround c d m size = -- Vide sous ses pieds
        Tombeur (State (snd $ hasGround c d m size) B 3) 0 st
    | d == D && getX c `mod` size == size - 1 =
        if estDure (persoCoordToMapCoord (bougeCoord d c) size) m
        then
            Marcheur st { direction = G }
        else
            Marcheur st { coord = bougeCoord D c }
    | not $ estDure (modifyCoord (persoCoordToMapCoord (bougeCoord d c) size) d) m = -- Vide là où il regarde
        Marcheur st { coord = bougeCoord d c }
    | estDure (modifyCoord (persoCoordToMapCoord (bougeCoord d c) size) d) m = -- Dure là où il regarde
        if d == G -- Regarde à gauche
        then
            if estDure (persoCoordToMapCoord (bougeCoord GH c) size) m || -- Dure en haut à gauche
               estDure (persoCoordToMapCoord (bougeCoord H c) size) m -- Dure en haut
            then
                Marcheur st { direction = D } -- Se retourne à droite
            else
                Marcheur st { coord = bougeCoord G (jump c d) } -- Avance en Haut à Gauche
        else -- Regarde à droite
            if estDure (modifyCoord (persoCoordToMapCoord (bougeCoord DH c) size) D) m || -- Dure en haut à droite
               estDure (persoCoordToMapCoord (bougeCoord H c) size) m -- Dure en haut
            then
                Marcheur st { direction = G } -- Se retourne à Gauche
            else
                Marcheur st { coord = bougeCoord D (jump c d) } -- Avance en Haut à Droite
    | otherwise = march
tourMarcheur st _ = st -- Ce n'est pas un Marcheur

tourTombeur :: Status -> Niveau -> Status
tourTombeur tomb@(Tombeur (State c d s) dist st) n@(Niveau _ _ size m _ infoN)
    | fst $ hasGround c d m size = --hasGround (persoCoordToMapCoord c size) m size = 
        if dist `div` size > getDistanceMortelle n -- Vérifie si la distance est mortelle
        then
            Mort (State c d s) -- Mort
        else
            Marcheur (State c (getDirectionFromState st) (getSpeedFromState st)) -- Devient Marcheur
    | otherwise = -- Continue de tomber
        Tombeur (State (bougeCoord B c) d s) (dist + 1) st
tourTombeur tomb _ = tomb -- Ce n'est pas un Tombeur

jump :: Coord -> Deplacement  -> Coord
jump (C x y) D = C (x + 1) (y - 20)
jump (C x y) G = C x (y - 20)

modifyCoord :: Coord -> Deplacement -> Coord
modifyCoord (C x y) d
    | d == D = C (x + 1) y
    | otherwise = C x y

hasGround :: Coord -> Deplacement -> Map Coord Case -> Int -> (Bool, Coord)
hasGround c@(C x y) d m size
    | x `mod` size == 0 =
        (estDure (bougeCoord B (persoCoordToMapCoord c size)) m, c)
    | d == D && x `mod` size == 19 =
        (estDure (bougeCoord DB (persoCoordToMapCoord c size)) m, C (x + 1) y)
    | otherwise = (estDure (bougeCoord B (persoCoordToMapCoord c size)) m || -- Sol sous ses pieds OU
        estDure (bougeCoord DB (persoCoordToMapCoord c size)) m, c) -- ET sol 

isFinish :: Game -> (Bool, GameStatus)
isFinish g@(Game n@(Niveau _ _ _ _ _ (InfoNiveau _ nbSpawn nNbExit _ _ _)) lemm infoG@(InfoGame gNbSpawn gNbExit gs) _)
    | gs == Win || gs == Loose = (True, gs) -- Si la partie est finie
    | gs == Playing = -- Si la partie est en cours
        if not (stillAlive lemm) && nbSpawn == gNbSpawn -- Si tous les lemmings sont morts
        then
            (True, Loose)
        else
            if gNbExit >= nNbExit -- Si le minimum est atteint
            then
                (True, Win) -- Renvoie True et le statut gagnant
            else
                (False, Playing) -- Renvoie False
    | otherwise = (False, Playing) -- Sinon, la partie continue
