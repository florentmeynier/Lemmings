module Mouse where

import SDL

import Model (GameState)
import qualified Model as M

import GameManager

import Movement

import Lemming

import Map

handleEvent :: [Event] -> Game -> Game
handleEvent [] g = g
handleEvent (e:q) g@(Game n lemm infoG cl) = 
    case eventPayload e of
        MouseButtonEvent be -> 
            case mouseButtonEventPos be of
            SDL.P (V2 pX pY) -> handleEvent q (aux (fromIntegral pX) (fromIntegral pY))
        _ -> handleEvent q g
    where
    aux :: Int -> Int -> Game
    aux pX pY 
        | pY < (getHauteur n * getSize n) = Game n (cliqueMap lemm cl pX pY) infoG cl
        | otherwise  = Game n lemm infoG (cliqueClasse pX (getSize n))

cliqueMap :: [Character] -> Int -> Int -> Int -> [Character]
cliqueMap [] _ _ _ = []
cliqueMap (c:q) cl pX pY     
    | posInCharacter c pX pY = 
        case cl of
            0 -> Lemming (getStatus c):q
            1 -> Flotteur (getStatus c):q
            2 -> Grimpeur (getStatus c):q
            3 -> Pelleteur (getStatus c):q
            4 -> Creuseur (getStatus c):q
    | otherwise = c:cliqueMap q cl pX pY

cliqueClasse :: Int -> Int -> Int
cliqueClasse pX size = pX `div` (size * 2)

posInCharacter :: Character -> Int -> Int -> Bool            
posInCharacter c x y = 
    let (C cX cY) = getCoordonnee c in
    x >= cX && x <= cX + 20 && y >= cY && y <= cY + 20