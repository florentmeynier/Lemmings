module SpriteMap where

import Data.Map.Strict (Map)
import qualified Data.Map as M

import SDL

import Sprite (Sprite)
import qualified Sprite as S

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

newtype SpriteId = SpriteId String
  deriving (Eq, Ord)

instance Show SpriteId where
  show (SpriteId id) = show id



type SpriteMap = Map SpriteId Sprite

createSpriteMap :: SpriteMap
createSpriteMap = M.empty

addSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
addSprite sid spr tmap =
  M.insertWithKey (\_ _ _ -> error $ "addSprite - Sprite '" <> (show sid) <> "' already in sprite map.")
  sid spr tmap

fetchSprite :: SpriteId -> SpriteMap -> Sprite
fetchSprite sid smap = case M.lookup sid smap of
                         Nothing -> error $ "fetchSprite - No such Sprite: " <> (show sid)
                         Just spr -> spr

updateSprite :: (Sprite -> Sprite) -> SpriteId -> SpriteMap -> SpriteMap
updateSprite f sid smap = M.alter aux sid smap
  where aux Nothing = error $ "updateSprite - No such sprite '" <> (show sid) <> "' in sprite map."
        aux (Just old) = Just $ f old

changeSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
changeSprite sid spr smap = updateSprite (\_ -> spr) sid smap

removeSprite :: SpriteId -> SpriteMap -> SpriteMap
removeSprite sid smap = case M.lookup sid smap of
                          Nothing -> error $ "removeSprite - No such sprite '" <> (show sid) <> "' in sprite map."
                          Just _ -> M.delete sid smap

loadSprite :: Renderer -> FilePath -> String -> TextureMap -> SpriteMap -> Int -> Int -> IO (TextureMap, SpriteMap)
loadSprite rdr path spriteId tmap smap h l = do
  let h' = fromIntegral h
  let l' = fromIntegral l
  tmap' <- TM.loadTexture rdr path (TextureId spriteId) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId spriteId) (S.mkArea 0 0 l' h')
  let smap' = addSprite (SpriteId spriteId) sprite smap
  return (tmap', smap')