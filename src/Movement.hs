module Movement where

data Coord = C Int Int 
    deriving(Show, Eq)

data Deplacement = N 
                  |G
                  |D
                  |H
                  |B
                  |GH 
                  |GB
                  |DH 
                  |DB 
    deriving(Show, Eq)

bougeCoord :: Deplacement -> Coord -> Coord
bougeCoord N c = c 
bougeCoord G (C x y) = C (x-1) y
bougeCoord D (C x y) = C (x+1) y
bougeCoord H (C x y) = C x (y-1)
bougeCoord B (C x y) = C x (y+1)
bougeCoord GH (C x y) = C (x-1) (y-1)
bougeCoord DH (C x y) = C (x+1) (y-1)
bougeCoord GB (C x y) = C (x-1) (y+1)
bougeCoord DB (C x y) = C (x+1) (y+1)

