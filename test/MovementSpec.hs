module MovementSpec where

import Test.Hspec

import Movement

initCoord = C 1 1

bougeCoordSpec = do
    describe "bougeCoord" $ do
        it "bouge G" $ do
            bougeCoord G initCoord `shouldBe` C 0 1
        it "bouge H" $ do
            bougeCoord D initCoord `shouldBe` C 1 0
        it "bouge G-D" $ do
            bougeCoord D (bougeCoord G initCoord) `shouldBe` initCoord

movementAllSpec = do
    bougeCoordSpec
