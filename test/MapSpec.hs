{-# LANGUAGE BlockArguments #-}
module MapSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Map as M

import Lemming
import Map 
import Movement
    
initNiveauS = Niveau 10 9 20 generateMapS M.empty (InfoNiveau 2 3 1 30 45 "test")
generateMapS = addEntreeSortie $ generateMap 10 9
addEntreeSortie m = M.insert (C 1 1) Entree (M.insert (C 7 8) Sortie m)

initNiveauSpec = do
    describe "initNiveau" $ do
        it "getHauteur" $ do
            getHauteur initNiveauS `shouldBe` 10
        it "getSize" $ do
            Map.getSize initNiveauS `shouldBe` 20
        it "getCassable" $ do
            getCassable initNiveauS `shouldBe` M.empty
        it "getDistanceMortelle" $ do
            getDistanceMortelle initNiveauS `shouldBe` 30
        it "getNiveauName" $ do
            getNiveauName initNiveauS `shouldBe` "test"


invNiveauSpec = do
    describe "invNiveau" $ do
        it "empty Map" $ do
            invNiveau (Niveau 10 10 20 M.empty M.empty (InfoNiveau 1 2 3 4 5 "test")) `shouldBe` False
        it "generateMap" $ do
            invNiveau initNiveauS `shouldBe` True
        it "getEntree" $ do
            getEntree initNiveauS `shouldBe` C 1 1

invEstDure = do
    describe "estDure" $ do
        it "Vide" $ do
            not $ estDure (C 2 2) generateMapS
        it "Entree" $ do
            not $ estDure (C 1 1) generateMapS
        it "Sortie" $ do
            not $ estDure (C 7 8) generateMapS
        it "Metal" $ do
            estDure (C 0 0) generateMapS
        it "Terre" $ do
            estDure (C 3 3) (M.insert (C 3 3) Metal generateMapS)

attaqueCaseSpec = do
    describe "attaqueCase" $ do
        it "Precondition True" $ do
            prop_attaqueCase_pre (C 3 3) (Niveau 10 10 20 (insert (C 3 3) Terre generateMapS) M.empty (InfoNiveau 1 2 3 4 5 "test"))
        it "Precondition False Vide" $ do
            not $ prop_attaqueCase_pre (C 3 3) initNiveauS
        it "Precondition False Metal" $ do
            not $ prop_attaqueCase_pre (C 0 0) initNiveauS

mapAllSpec = do
    initNiveauSpec
    invNiveauSpec
    invEstDure
    attaqueCaseSpec
