{-
 - NON-DETERMINISTIC TESTS FOR QUANTUMDRACULA
 -}
module NonDeterministic where

import Game
import QuantumDracula

import Control.Monad.Trans.State.Lazy (evalStateT, execStateT, modify)
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.List (sortBy)
import Data.Function (on)
import Data.Ord (Down(..))


main :: IO ()
main = do
    putStrLn "Scenario 1."
    putStrLn "  Dracula can bite two players (1000 runs)."
    putStrLn "  Should do it EVERY the time."
    scenario1 1000 >>= print
    putStrLn ""
    
    putStrLn "Scenario 2."
    putStrLn "  Total number of bites as aggressiveness increases (1000 runs per aggressiveness level)."
    scenario2Summary >>= print
    putStrLn ""

    putStrLn "Scenario 3."
    putStrLn "  Do the best bite in Scenario 2. Ask if Dracula is present in the Cellar (1000 runs)."
    putStrLn "  Say no almost all of the time."
    scenario3 1000 >>= print
    putStrLn ""

    putStrLn "Scenario 4."
    putStrLn "  Do the best bite in Scenario 2. Ask if Dracula is present in the South Hallway (1000 runs)."
    putStrLn "  Say no almost all of the time."
    scenario4 1000 >>= print
    putStrLn ""

-- Can bite two players, so always do it
scenario1 :: Int -> IO [(Room, Int)]
scenario1 = scenarioResults (draculaTurn st1) dist1
    where
        dist1 = [ Dungeon, Dining, Bathroom, Vent, Canal ]
        st1 = GameState
            { sunlights = [ Sunlight {castTo = Alley, castFrom = Gallery} ]
            , positions = Right <$> [ Gallery, Staircase, Tomb, Cellar ]
            , lastInfo = 2
            , lastBite = 2
            , canBite = True
            }


-- For me, it seems that the best bite is in SHall
scenario2 :: Int -> Int -> IO [(Room, Int)]
scenario2 lastBiteTurns = scenarioResults (draculaTurn st) dist
    where
        dist = [ Dungeon, Dining, Bathroom, Vent, Canal ]
        st = GameState
            { sunlights = [ Sunlight {castTo = Alley, castFrom = Gallery} ]
            , positions = Right <$> [ Gallery, Staircase, Tomb, SHall ]
            , lastInfo = 2
            , lastBite = lastBiteTurns
            , canBite = True
            }


-- ran this and got [391,624,726,818,854]
scenario2Summary :: IO [Int]
scenario2Summary = traverse (fmap (sum . map snd) . flip scenario2 1000) [1..5]


-- Turn immediately after doing a bite in SHall in scenario2. Dracula should
-- say False to presence in Cellar almost all the time
scenario3 :: Int -> IO [(Bool, Int)]
scenario3 = scenarioResults (pure <$> isPresent st Cellar) dist
    where
        dist = [ Cellar, SHall, Passage ]
        st = GameState
            { sunlights = []
            , positions = Left SHall : (Right <$> [ Gallery, Cellar, Tomb ])
            , lastInfo = 1
            , lastBite = 1
            , canBite = True
            }

-- Turn immediately after doing a bite in SHall in scenario2. Dracula should
-- say False to presence in SHall almost all the time
scenario4 :: Int -> IO [(Bool, Int)]
scenario4 = scenarioResults (pure <$> isPresent st SHall) dist
    where
        dist = [ Cellar, SHall, Passage ]
        st = GameState
            { sunlights = []
            , positions = Left SHall : (Right <$> [ Gallery, Cellar, Tomb ])
            , lastInfo = 1
            , lastBite = 1
            , canBite = True
            }


-- Run the ai `run` `n` times (with initial dracula state `dist`) and return a
-- collation of the results
scenarioResults :: Eq a => DraculaState [a] -> [Room] -> Int -> IO [(a, Int)]
scenarioResults run dist n = sortBy (compare `on` (Down . snd))
    <$>  execStateT (replicateM n singleScenario) []
    where
        singleScenario = lift (evalStateT run dist) >>= traverse update

        -- update the count of tuples
        update a = modify $ \st -> case splitWhen ((== a) . fst) st of
            (seen, []) -> (a, 1) : seen
            (seen, (a', count):rest) -> (a', count + 1) : seen ++ rest

        -- fast computation of (takeWhile (not . p) xs, dropWhile (not . p) xs)
        splitWhen _ [] = ([], [])
        splitWhen p (x:xs)
            | p x = ([], x:xs)
            | otherwise = first (x:) $ splitWhen p xs
