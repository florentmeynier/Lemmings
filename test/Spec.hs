import Test.Hspec

import MovementSpec as MoveS

main :: IO ()
main = hspec $ do
    MoveS.movementAllSpec 
