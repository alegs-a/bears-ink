{-
 - DETERMININISTIC TESTS FOR QUANTUMDRACULA
 -}
module Dterministic where

import Game
import QuantumDracula

import Test.Hspec
import Test.QuickCheck
import Control.Monad.Trans.State.Lazy (StateT(runStateT))
import Data.List ((\\))


test_turnStarts :: Spec
test_turnStarts = describe "Turn starts in example scenarios" $ do
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


test_shortest :: Spec
test_shortest = do
    describe "Properties of shortest" $ do
        it "Removing a possible starting room cannot make the shortest distance shorter" $ let
                prop :: [Room] -> Room -> Room -> NonEmptyList Room -> Bool
                prop inacc end room dist = shortest inacc end (room:dist') <= shortest inacc end dist'
                    where dist' = getNonEmpty dist
            in property prop

        it "If no path exists, the shortest distance is greater than DraculaMoves" $ let
                prop :: [Room] -> Room -> NonEmptyList Room -> Bool
                prop inacc end dist = elem end dist' || shortest (end:inacc) end dist' > draculaMoves
                    where dist' = getNonEmpty dist
            in property prop

        it "Shortest path to the current room is always 0" $ let
                prop :: [Room] -> Room -> [Room] -> Bool
                prop inacc room dist = shortest (filter (/= room) inacc) room (room:dist) == 0
            in property prop

        it "Shortest path to everything in walkEnds" $ let
                prop :: [Room] -> NonEmptyList Room -> Bool
                prop inacc dist = all
                    (\len -> all (withinDistance len) $ walkEnds inacc' len dist')
                    [0..3]
                    where
                        dist' = getNonEmpty dist
                        inacc' = unique inacc \\ dist'
                        withinDistance n end = shortest inacc' end dist' <= n
            in property prop

        it "Symmetry" $ let
                prop inacc r1 r2 = shortest inacc' r1 [r2] == shortest inacc' r2 [r1]
                    where inacc' = unique inacc \\ [r1, r2]
            in property prop

    describe "Specific examples of shortest" $ do
        it "Tomb and Dungeon" $ shortest [] Tomb [Dungeon] `shouldBe` 3
        it "Dungeon and Gallery" $ do
            shortest [] Gallery [Dungeon] `shouldBe` 2
            shortest [Alley] Gallery [Dungeon] `shouldBe` 3
