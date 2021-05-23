module GameManagerSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Map as M

import GameManager
import Map
import Movement
import Lemming

initGameManagerS = Game initNiveauS [] initInfoGameS 0
initNiveauS = Niveau 10 9 20 generateMapS M.empty (InfoNiveau 120 3 1 3 45 "test")
generateMapS = addEntreeSortie $ generateMap 10 9
addEntreeSortie m = M.insert (C 1 1) Entree (M.insert (C 7 8) Sortie (M.insert (C 1 8) Terre m))
initLemmingS = [Lemming $ Marcheur $ State (C 138 160) D 1, Flotteur $ Tombeur (State (C 60 60) B 1) 0 (State (C 3 3) D 1), Creuseur $ Mort $ State (C 100 160) G 1]
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
            spawnCharacter initGameManagerS 120 `shouldBe` 
                Just (Lemming (Marcheur (State (C 20 20) D 1)))

hasGroundSpec = do
    describe "hasGround" $ do
        it "True with x mod size == 0" $ do
            hasGround (C 140 160) D generateMapS 20 `shouldBe` (True, C 140 160)
        it "True with x mod size == 19" $ do
            hasGround (C 139 160) D generateMapS 20 `shouldBe` (True, C 140 160)
        it "True otherwise" $ do
            hasGround (C 138 160) G generateMapS 20 `shouldBe` (True, C 138 160)
        it "False with x mod size == 0" $ do
            hasGround (C 140 140) D generateMapS 20 `shouldBe` (False, C 140 140)
        it "True with x mod size == 19" $ do
            hasGround (C 139 140) D generateMapS 20 `shouldBe` (False, C 140 140)
        it "False otherwise" $ do
            hasGround (C 138 140) G generateMapS 20 `shouldBe` (False, C 138 140)

tourTombeurSpec = do
    describe "Precondition tourTombeur" $ do
        it "True" $ do
            prop_tourTombeur_pre $ Tombeur (State (C 0 0) B 1) 0 (State (C 0 0) D 1) 
        it "False" $ do
            not $ prop_tourTombeur_pre $ Marcheur $ State (C 0 0) B 1
    describe "tourTombeur" $ do
        it "Hit the ground alive" $ do
            tourTombeur (Tombeur (State (C 140 160) B 1) 1 (State (C 140 158) D 2)) initNiveauS 
                `shouldBe` Marcheur (State (C 140 160) D 2)
        it "Hit the ground dead" $ do
            tourTombeur (Tombeur (State (C 140 160) B 1) 81 (State (C 140 158) D 2)) initNiveauS 
                `shouldBe` Mort (State (C 140 160) B 1)
        it "Continue to Fall" $ do
            tourTombeur (Tombeur (State (C 140 159) B 1) 1 (State (C 140 158) D 2)) initNiveauS 
                `shouldBe` Tombeur (State (C 140 160) B 1) 2 (State (C 140 158) D 2) 
                
tourMarcheurSpec = do
    describe "Precondition tourMarcheur" $ do
        it "True" $ do
            prop_tourMarcheur_pre $ Marcheur (State (C 1 1) D 1)
        it "False" $ do
            not $ prop_tourMarcheur_pre $ Mort (State (C 1 1) D 1)
    describe "tourMarcheur" $ do
        it "No ground detected, has to fall" $ do
            tourMarcheur (Marcheur (State (C 30 30) D 1)) initNiveauS `shouldBe` 
                Tombeur (State (C 30 30) B 3) 0 (State (C 30 30) D 1)
        it "Wall detected, has to turn around" $ do
            tourMarcheur (Marcheur (State (C 140 160) D 1)) initNiveauS `shouldBe` 
                Marcheur (State (C 140 160) G 1)
        it "Has to climb" $ do
             tourMarcheur (Marcheur (State (C 40 160) G 1)) initNiveauS `shouldBe` 
                Marcheur (State (C 39 140) G 1)

tourCreuseurSpec = do
    describe "Precondition tourCreuseur" $ do
        it "True" $ do
            prop_tourCreuseur_pre $ Creuseur $ Marcheur (State (C 1 1) D 1)
        it "False" $ do
            not $ prop_tourCreuseur_pre $ Lemming $ Marcheur (State (C 1 1) D 1)
    describe "tourCreuseur" $ do
        it "Nothing to do, tourFinish" $ do
            let creuseur = Creuseur (Marcheur (State (C 100 160) D 1))
            tourCreuseur creuseur initNiveauS 0 `shouldBe` (creuseur, initNiveauS)
        it "Nothing to do, has to walk" $ do
            tourCreuseur (Creuseur (Marcheur (State (C 100 160) D 1))) initNiveauS 1 
                `shouldBe` (Creuseur (Marcheur (State (C 101 160) D 1)), initNiveauS)
        it "Has to dig 1 time" $ do
            getCassable (snd (tourCreuseur (Creuseur (Marcheur (State (C 20 140) G 1))) initNiveauS 1))
                `shouldBe` M.fromList [(C 1 8, 44)]
        it "Has to dig 45 times, block destroy" $ do
            getCassable (snd (tourCreuseur (Creuseur (Marcheur (State (C 20 140) G 1))) initNiveauS 45))
                `shouldBe` M.empty;

tourPelleteurSpec = do
    describe "Precondition tourPelleteur" $ do
        it "True" $ do
            prop_tourPelleteur_pre $ Pelleteur $ Marcheur (State (C 1 1) D 1)
        it "False" $ do
            not $ prop_tourPelleteur_pre $ Lemming $ Marcheur (State (C 1 1) D 1)    
    describe "tourPelleteur" $ do
        it "Nothing to do, tourFinish" $ do
            let pelleteur = Pelleteur (Marcheur (State (C 100 160) D 1))
            tourPelleteur pelleteur initNiveauS 0 `shouldBe` (pelleteur, initNiveauS) 
        it "Nothing to do, has to walk" $ do
            tourPelleteur (Pelleteur (Marcheur (State (C 100 160) D 1))) initNiveauS 1 
                `shouldBe` (Pelleteur (Marcheur (State (C 101 160) D 1)), initNiveauS)
        it "Has to dig 1 time" $ do
            getCassable (snd (tourPelleteur (Pelleteur (Marcheur (State (C 40 160) G 1))) initNiveauS 1))
                `shouldBe` M.fromList [(C 1 8, 44)]
        it "Has to dig 45 times, block destroy" $ do
            getCassable (snd (tourPelleteur (Pelleteur (Marcheur (State (C 40 160) G 1))) initNiveauS 45))
                `shouldBe` M.empty;

tourGrimpeurSpec = do
    describe "Precondition tourGrimpeur" $ do
        it "True" $ do
            prop_tourGrimpeur_pre $ Grimpeur (Marcheur (State (C 1 1) D 1)) True
        it "False" $ do
            not $ prop_tourGrimpeur_pre $ Lemming $ Marcheur (State (C 1 1) D 1)  
    describe "tourGrimpeur" $ do
        it "Nothing to do, tourFinish" $ do
            let grimpeur = Grimpeur (Marcheur (State (C 100 160) D 1)) False
            tourGrimpeur grimpeur initNiveauS 0 `shouldBe` grimpeur 
        it "Nothing to do, has reach the roof" $ do
            let grimpeur = Grimpeur (Marcheur (State (C 139 20) D 1)) True
            tourGrimpeur grimpeur initNiveauS 0 `shouldBe` grimpeur 
        it "Has to climb 1 time" $ do
            tourGrimpeur (Grimpeur (Marcheur (State (C 139 160) D 1)) False) initNiveauS 1
                `shouldBe` Grimpeur (Marcheur (State (C 139 159) D 1)) True
        it "Has to climb 20 times" $ do
            tourGrimpeur (Grimpeur (Marcheur (State (C 139 160) D 1)) False) initNiveauS 20
                `shouldBe` Grimpeur (Marcheur (State (C 139 140) D 1)) True

tourFlotteurSpec = do
    describe "Precondition tourFlotteur" $ do
        it "True" $ do
            prop_tourFlotteur_pre $ Flotteur (Marcheur (State (C 1 1) D 1))
        it "False" $ do
            not $ prop_tourFlotteur_pre $ Lemming $ Marcheur (State (C 1 1) D 1)
    describe "tourFlotteur" $ do
        it "Has to walk" $ do
            tourFlotteur (Flotteur (Marcheur (State (C 120 160) G 1))) initNiveauS 1 
                `shouldBe` Flotteur (Marcheur (State (C 119 160) G 1))
        it "Has to fall 1 time" $ do
            tourFlotteur (Flotteur (Tombeur (State (C 50 50) G 1) 0 (State (C 50 50) G 1))) initNiveauS 1
                `shouldBe` Flotteur (Tombeur (State (C 50 51) G 1) 0 (State (C 50 50) G 1))
        it "Has to fall 50 times" $ do
            tourFlotteur (Flotteur (Tombeur (State (C 50 50) G 1) 0 (State (C 50 50) G 1))) initNiveauS 50
                `shouldBe` Flotteur (Tombeur (State (C 50 100) G 1) 0 (State (C 50 50) G 1))
            
tourLemmingSpec = do
    describe "tourLemming" $ do
        it "Has to walk" $ do
            let lemming = Lemming $ Marcheur $ State (C 120 120) D 1
            tourLemming lemming initNiveauS 1 `shouldBe` Lemming (tourMarcheur (getStatus lemming) initNiveauS)
        it "Wall detected, has to turn around" $ do
            let lemming = Lemming (Marcheur (State (C 140 160) D 1)) 
            tourLemming lemming initNiveauS 1
                `shouldBe` Lemming (tourMarcheur (getStatus lemming) initNiveauS)
        it "Has to climb" $ do
            let lemming = Lemming (Marcheur (State (C 40 160) G 1)) 
            tourLemming lemming initNiveauS 1 
                `shouldBe` Lemming (tourMarcheur (getStatus lemming) initNiveauS)

gameStepLemmingsSpec = do
    describe "gameStepLemmings" $ do
        it "Empty list" $ do
            gameStepLemmings [] initNiveauS `shouldBe` ([], initNiveauS)
        it "1 step" $ do
            fst (gameStepLemmings initLemmingS initNiveauS) 
                `shouldBe` [Lemming $ Marcheur $ State (C 139 160) D 1, Flotteur $ Tombeur (State (C 60 61) B 1) 0 (State (C 3 3) D 1), Creuseur $ Mort $ State (C 100 160) G 1]


gameManagerAllSpec = do
    spawnCharacterSpec
    hasGroundSpec
    tourTombeurSpec
    tourMarcheurSpec
    tourCreuseurSpec
    tourPelleteurSpec
    tourGrimpeurSpec
    tourFlotteurSpec
    tourLemmingSpec
    gameStepLemmingsSpec