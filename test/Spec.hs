import Test.Hspec

import MovementSpec as MoveS
import LemmingSpec as LemmingS
import MapSpec as MapS
import GameManagerSpec as GameS

main :: IO ()
main = hspec $ do
    MoveS.movementAllSpec 
    LemmingS.lemmingAllSpec
    MapS.mapAllSpec
    GameS.gameManagerAllSpec