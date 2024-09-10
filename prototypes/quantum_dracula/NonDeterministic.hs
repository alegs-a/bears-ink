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


-- Run the ai `run` `n` times (with initial dracula state `dist`) and return a
-- collation of the results
scenarioResults :: Eq a => DraculaState [a] -> [Room] -> Int -> IO [(a, Int)]
scenarioResults run dist n = execStateT (replicateM n singleScenario) []
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
