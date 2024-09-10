{-
 - DETERMININISTIC TESTS FOR QUANTUMDRACULA
 -}
module Dterministic where

import Game
import QuantumDracula

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Trans.State.Lazy (StateT(runStateT))


test_turnStarts :: Spec
test_turnStarts = do
    describe "Example scenarios" $ do
        let stTemplate = GameState
                { sunlights = []
                , positions = pure <$> [ Dining, SHall, Cellar, Ballroom ]
                , lastInfo = 1
                , lastBite = 3
                , canBite = True
                }

        it "Scenario 1: No moves available, but no sunlight in current position" $ do
            let initDist = [ Staircase ]
            let st = stTemplate
                    { sunlights = 
                        [ Sunlight { castTo = Ballroom, castFrom = Dining }
                        , Sunlight { castTo = Cellar, castFrom = SHall }
                        ]
                    }
            turnStarts st initDist `shouldBe` ( initDist, draculaMoves )
            runStateT (draculaTurn st) initDist `shouldReturn` ( [], initDist )

        it "Scenario 2: No moves available and sunlight in current position" $ do
            let initDist = [ Staircase ]
            let st = stTemplate
                    { sunlights =
                        [ Sunlight { castTo = Ballroom, castFrom = Dining }
                        , Sunlight { castTo = Staircase, castFrom = Cellar }
                        ]
                    }
            turnStarts st initDist `shouldBe` ( initDist, 0 )
            runStateT (draculaTurn st) initDist `shouldReturn` ( [], initDist )

        -- it "Scenario 3: First round" $ do
            -- TODO: generate a random list of starting places. Ensure that
            -- validMoves consist of all and only the rooms that are within one
            -- space of the starting position
