{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

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

import Movement (Coord, Deplacement)
import qualified Movement as Move

import Lemming (State)
import qualified Lemming as L

import Map

import GameManager (Game)
import qualified GameManager as GM

loadBackground :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadGround :: Renderer -> FilePath -> TextureMap -> SpriteMap -> Int -> IO (TextureMap, SpriteMap)
loadGround rdr path tmap smap size = do
  let size' = fromIntegral size
  tmap' <- TM.loadTexture rdr path (TextureId "metal") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "metal") (S.mkArea 0 0 size' size')
  let smap' = SM.addSprite (SpriteId "metal") sprite smap
  return (tmap', smap')

loadEntreeDoor :: Renderer -> FilePath -> TextureMap -> SpriteMap -> Int -> IO (TextureMap, SpriteMap)
loadEntreeDoor rdr path tmap smap size = do
  let size' = fromIntegral size
  tmap' <- TM.loadTexture rdr path (TextureId "eDoor") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "eDoor") (S.mkArea 0 0 size' size')
  let smap' = SM.addSprite (SpriteId "eDoor") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> Int -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap size = do
  let size' = fromIntegral size
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 size' size')
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadVirus :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVirus rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "virus") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "virus") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "virus") sprite smap
  return (tmap', smap')

main :: IO ()
main = do
  initializeAll
  -- initialisation de la partie
  let l = 8 -- 32 :: Int
  let h = 24 :: Int
  let size = 20 :: Int
  let game = GM.initGameManager h l size 20 50 Move.D 1
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 (fromIntegral (l * size)) (fromIntegral (h * size)) }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  --(tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  (tmap, smap) <- SM.loadSprite renderer "assets/background.bmp" "background" TM.createTextureMap SM.createSpriteMap 480 640
  -- chargement du metal
  (tmap, smap) <- SM.loadSprite renderer "assets/metal.bmp" "metal" tmap smap size size
  -- chargement de la terre
  (tmap, smap) <- SM.loadSprite renderer "assets/terre.bmp" "terre" tmap smap size size
  -- chargement de la porte
  (tmap, smap) <- SM.loadSprite renderer "assets/eDoor.bmp" "eDoor" tmap smap size size
  -- chargement du personnage
  (tmap, smap) <- SM.loadSprite renderer "assets/perso.bmp" "perso" tmap smap size size
  -- initialisation de l'état du jeu
  vx <- R.randomRIO(0,640)
  vy <- R.randomRIO(0,480)
  let gameState = M.initGameState vx vy
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd gameState game

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> Game -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState gm = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  MS.handleEvent events gameState
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --- display perso 
  --S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
  --                               (fromIntegral (M.persoX gameState))
  --                               (fromIntegral (M.persoY gameState)))                        

  --S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "ground") smap)
                                --50 50)
 
  displayPerso renderer tmap smap gm
  displayMap renderer tmap smap gm

  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep gameState kbd' deltaTime
  --let st' = L.gameStepL st
  let gm' = GM.gameStepGM gm
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' gm')

displayPerso :: Renderer -> TextureMap -> SpriteMap -> Game -> IO ()
displayPerso renderer tmap smap (GM.Game _ lemmings) = head $ aux lemmings where
  aux :: [L.Characters] -> [IO ()]
  aux [] = [return ()]
  aux (c:q) = S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral $ L.getCoordX c)
                                 (fromIntegral $ L.getCoordY c)):aux q

displayMap :: Renderer -> TextureMap -> SpriteMap -> Game -> IO ()
displayMap renderer tmap smap gm@(GM.Game n@(Niveau h l size m) lemm) =
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
        (Move.C x y, Terre) -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "terre") smap)
                                  (fromIntegral (size * x))
                                  (fromIntegral (size * y)))
        _ -> return ()
      aux q
