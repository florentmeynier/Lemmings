module GameManagerSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Map as M

import GameManager
import Map
import Movement
import Lemming

initGameManagerS = Game initNiveauS [] initInfoGameS 0
initNiveauS = Niveau 10 9 20 generateMapS M.empty (InfoNiveau 2 3 1 30 45 "test")
generateMapS = addEntreeSortie $ generateMap 10 9
addEntreeSortie m = M.insert (C 1 1) Entree (M.insert (C 7 8) Sortie m)
initLemmingS = [Lemming $ Marcheur $ State (C 0 0) D 1, Flotteur $ Tombeur (State (C 3 3) B 1) 0 (State (C 3 3) D 1), Creuseur $ Mort $ State (C 1 1) G 1]
initInfoGameS = InfoGame 0 0 Playing 

initGameManagerS2 = Game initNiveauS initLemmingS initInfoGameS2 0 
initInfoGameS2 = InfoGame 3 0 Playing 

spawnCharacterSpec = do 
    describe "spawnCharacterPrecondition" $ do
        it "Precondition True" $ do
            prop_spawnCharacter_pre initGameManagerS
        it "Precondition False" $ do
            not $ prop_spawnCharacter_pre initGameManagerS2
    describe "spawnCharacter" $ do
        it "spawnCharacter with maxSpawn reach" $ do
            spawnCharacter initGameManagerS2 0 `shouldBe` Nothing
        it "spawnCharacter with wrong ticks" $ do
            spawnCharacter initGameManagerS 2 `shouldBe` Nothing
        it "spawnCharacter" $ do
            spawnCharacter initGameManagerS 0 `shouldBe` 
                Just (Lemming (Marcheur (State (C 1 1) D 1)))
        

gameManagerAllSpec = do
    spawnCharacterSpec
