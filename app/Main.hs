{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl', isPrefixOf)

import Data.Map as DMap

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M

import qualified System.Random as R

import Mouse
import qualified Mouse as MS

import File

import Movement (Coord, Deplacement)
import qualified Movement as Move

import Lemming (State)
import qualified Lemming as L

import Map

import GameManager (Game)
import qualified GameManager as GM

loadAllSprites :: Renderer -> Int -> Int -> Int -> IO (TextureMap, SpriteMap)
loadAllSprites renderer h l size = do
  let tmap = TM.createTextureMap
  let smap = SM.createSpriteMap
  -- chargement de l'image du fond
  (tmap, smap) <- SM.loadSprite renderer "assets/background.bmp" "background" TM.createTextureMap SM.createSpriteMap 480 640
  -- chargement du metal
  (tmap, smap) <- SM.loadSprite renderer "assets/metal.bmp" "metal" tmap smap size size
  -- chargement de la terre
  (tmap, smap) <- SM.loadSprite renderer "assets/terre.bmp" "terre" tmap smap size size
  -- chargement de la porte d'entrée
  (tmap, smap) <- SM.loadSprite renderer "assets/eDoor.bmp" "eDoor" tmap smap size size
  -- chargement de la sortie
  (tmap, smap) <- SM.loadSprite renderer "assets/sDoor.bmp" "sDoor" tmap smap size size
  -- chargement du fond blanc
  (tmap, smap) <- SM.loadSprite renderer "assets/white.bmp" "white" tmap smap (2 * size) (l * size)
  -- chargement class Lemming
  (tmap, smap) <- SM.loadSprite renderer "assets/redcross.bmp" "redcross" tmap smap size size
  -- chargement du parapluie
  (tmap, smap) <- SM.loadSprite renderer "assets/umbrella.bmp" "umbrella" tmap smap size size
  -- chargement du piolet
  (tmap, smap) <- SM.loadSprite renderer "assets/piolet.bmp" "piolet" tmap smap size size
  -- chargement de la pelleteuse
  (tmap, smap) <- SM.loadSprite renderer "assets/pelleteuse.bmp" "pelleteuse" tmap smap size size
  -- chargement d'un contour
  (tmap, smap) <- SM.loadSprite renderer "assets/contour.bmp" "contour" tmap smap (2 * size) (2 * size)
  -- chargement de la pelle
  (tmap, smap) <- SM.loadSprite renderer "assets/pelle.bmp" "pelle" tmap smap size size
  -- chargement du personnage
  (tmap, smap) <- SM.loadSprite renderer "assets/perso.bmp" "perso" tmap smap size size
  pure (tmap, smap)

main :: IO ()
main = do
  initializeAll
  selectGameModeLoop
  -- initialisation de la partie
  niveau@(Niveau h l size _ _ _) <- loadNiveau "maps/solo4"
  let size = getSize niveau
  let game = GM.Game niveau [] GM.initInfoGame 0
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 (fromIntegral (l * size)) (fromIntegral ((h + 2) * size)) }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de toutes les textures
  (tmap, smap) <- loadAllSprites renderer h l size
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd game 1

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Game -> Int -> IO ()
gameLoop frameRate renderer tmap smap kbd game tick = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  let game1 = MS.handleEvent events game
  clear renderer
  display renderer tmap smap game1
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  let game' = GM.gameStepGameManager game1 tick
  case GM.isFinish game' of
    (True, GM.Win) -> -- Si la partie est gagnée
      if isPrefixOf "solo" (Map.getNiveauName $ GM.getNiveau game')
      then -- Si c'est un niveau du solo
        nextLevel frameRate renderer tmap smap kbd' (GM.getNiveau game')
      else -- Si c'est un niveau importé
        return ()
    (False, _) ->
      if K.keypressed KeycodeR kbd'
      then 
          startNewGame frameRate renderer tmap smap kbd' (GM.getNiveau game')
      else
        if K.keypressed KeycodeEscape kbd'
        then
          return ()
        else
          gameLoop frameRate renderer tmap smap kbd' game' (tick + 1)
    _ -> return ()

nextLevel frameRate renderer tmap smap kbd' n = do
  let (h1, h2) = Prelude.splitAt 4 (getNiveauName n)
  let lvl = read h2 :: Int
  n' <- loadNiveau $ "maps/solo" ++ show (lvl + 1)
  let gm = GM.Game n' [] GM.initInfoGame 0
  gameLoop frameRate renderer tmap smap kbd' gm 0

startNewGame frameRate renderer tmap smap kbd' n = do
  n' <- loadNiveau $ "maps/" ++ getNiveauName n
  let gm = GM.Game n' [] GM.initInfoGame 0
  gameLoop frameRate renderer tmap smap kbd' gm 0

selectGameModeLoop :: IO ()
selectGameModeLoop = do
  putStrLn "Choisi ton mode de jeu :"

display :: Renderer -> TextureMap -> SpriteMap -> Game -> IO ()
display renderer tmap smap game = do
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap) -- affiche le fond
  displayPerso renderer tmap smap game -- affiche les Lemmings
  displayMap renderer tmap smap game -- affiche les murs
  displayClasse renderer tmap smap game -- affiche le menu de sélection de classe

displayPerso :: Renderer -> TextureMap -> SpriteMap -> Game -> IO ()
displayPerso renderer tmap smap (GM.Game _ lemmings _ _) = aux lemmings where
  aux :: [L.Character] -> IO ()
  aux [] = return ()
  aux (c:q) = do
    case L.getStatus c of -- Vérifie le statut du Lemming
      L.Mort _ -> return () -- Si il est mort, n'affiche rien
      _ -> do
        S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral $ L.getCoordX c)
                                 (fromIntegral $ L.getCoordY c))
        case c of
          L.Flotteur _ -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "umbrella") smap)
                                 (fromIntegral $ L.getCoordX c)
                                 (fromIntegral $ L.getCoordY c))
          L.Grimpeur _ -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "piolet") smap)
                                 (fromIntegral $ L.getCoordX c)
                                 (fromIntegral $ L.getCoordY c))
          L.Pelleteur _ -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "pelleteuse") smap)
                                 (fromIntegral $ L.getCoordX c)
                                 (fromIntegral $ L.getCoordY c))
          L.Creuseur _ -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "pelle") smap)
                                 (fromIntegral $ L.getCoordX c)
                                 (fromIntegral $ L.getCoordY c))
          _ -> return ()
    aux q

displayMap :: Renderer -> TextureMap -> SpriteMap -> Game -> IO ()
displayMap renderer tmap smap gm@(GM.Game n@(Niveau h l size m _ _) lemm _ _) =
    aux (toList m)
  where
    aux :: [(Coord, Case)] -> IO ()
    aux [] = return ()
    aux (h:q) = do
      case h of
        (Move.C x y, Metal) -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "metal") smap)
                                  (fromIntegral (size * x))
                                  (fromIntegral (size * y)))
        (Move.C x y, Entree) -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "eDoor") smap)
                                  (fromIntegral (size * x))
                                  (fromIntegral (size * y)))
        (Move.C x y, Sortie) -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "sDoor") smap)
                                  (fromIntegral (size * x))
                                  (fromIntegral (size * y)))
        (Move.C x y, Terre) -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "terre") smap)
                                  (fromIntegral (size * x))
                                  (fromIntegral (size * y)))
        _ -> return ()
      aux q

displayClasse :: Renderer -> TextureMap -> SpriteMap -> Game -> IO ()
displayClasse renderer tmap smap gm@(GM.Game n@(Niveau h l size m _ _) lemm _ cl) = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "white") smap)
                                  0 (fromIntegral $ h * size))
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "redcross") smap)
                                  (fromIntegral $ size `div` 2) (fromIntegral $ h * size + (size `div` 2)))
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "umbrella") smap)
                                  (fromIntegral $ (1 * (size * 2)) + (size `div` 2)) (fromIntegral $ h * size + (size `div` 2)))
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "piolet") smap)
                                  (fromIntegral $ (2 * (size * 2)) + (size `div` 2)) (fromIntegral $ h * size + (size `div` 2)))
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "pelleteuse") smap)
                                  (fromIntegral $ (3 * (size * 2)) + (size `div` 2)) (fromIntegral $ h * size + (size `div` 2)))
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "pelle") smap)
                                  (fromIntegral $ (4 * (size * 2)) + (size `div` 2)) (fromIntegral $ h * size + (size `div` 2)))

  if cl == -1
  then
    return ()
  else
    S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "contour") smap)
                                  (fromIntegral $ cl * size * 2) (fromIntegral $ h * size))
