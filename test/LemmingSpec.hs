{-# LANGUAGE BlockArguments #-}
module LemmingSpec where

import Test.Hspec
import Test.QuickCheck

import Lemming

import Movement

initMort = Lemming $ Mort $ State (C 0 0) D 1
initMarcheur = Lemming $ Marcheur $ State (C 1 1) G 1

initStateS = State (C 0 1) D 1

stillAliveSpec = do
    describe "Still alive" $ do
        it "empty list" $ do
            stillAlive [] `shouldBe` False
        it "List with dead" $ do
            stillAlive [initMort] `shouldBe` False
        it "List" $ do
            stillAlive [initMort, initMarcheur] `shouldBe` True

stateSpec = do
    describe "State test" $ do
        it "initState" $ do
            initState 0 1 D 1 `shouldBe` initStateS

lemmingAllSpec = do
    stillAliveSpec
    stateSpec
