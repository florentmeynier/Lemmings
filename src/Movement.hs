module Movement where

data Coord = C Int Int 
    deriving(Show, Eq)

data Deplacement = N 
                  | G
                  | D
                  | H
                  | B
                  | GH 
                  | GB
                  | DH 
                  | DB 
    deriving(Show, Eq)

getX :: Coord -> Int
getX (C x _) = x

getY :: Coord -> Int
getY (C _ y) = y

persoCoordToMapCoord :: Coord -> Int -> Coord
persoCoordToMapCoord (C x y) size = C (x `div` size) (y `div` size)

mapCoordToPersoCoord :: Coord -> Int -> Coord
mapCoordToPersoCoord (C x y) size = C (x * size) (y * size)

-- >>> persoCoordToMapCoord (C 15 23) 10
-- C 1 2

bougeCoord :: Deplacement -> Coord -> Coord
bougeCoord N c = c 
bougeCoord G (C x y) = C (x-1) y
bougeCoord D (C x y) = C (x+1) y
bougeCoord H (C x y) = C x (y-1)
bougeCoord B (C x y) = C x (y+1)
bougeCoord GH (C x y) = C (x-1) (y-1)
bougeCoord GB (C x y) = C (x-1) (y+1)
bougeCoord DH (C x y) = C (x+1) (y-1)
bougeCoord DB (C x y) = C (x+1) (y+1)

instance Ord Coord where
    (<) (C x1 y1) (C x2 y2) = if y1 == y2 then x1 < x2 else y1 < y2 
    (<=) (C x1 y1) (C x2 y2) = if y1 == y2 then x1 <= x2 else y1 < y2
    (>) (C x1 y1) (C x2 y2) = if y1 == y2 then x1 > x2 else y1 > y2
    (>=) (C x1 y1) (C x2 y2) = if y1 == y2 then x1 >= x2 else y1 > y2
    max c1@(C x1 y1) c2@(C x2 y2) = if c1 >= c2 then c1 else c2
