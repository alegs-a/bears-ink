module Nosferatu where

import Game
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Bifunctor (first, second)


-- List of possible player turns, given a starting state
playerMoves :: GameState -> [PlayerTurn]
playerMoves startState = do
    (resting, acting) <- actionPartition
    moves <- traverse (walks 1 . (positions startState !)) acting
    undefined
    where
        playerGarlic r = filter
            (not . flip Vector.elem (positions startState))
            (roomCloud 2 r)


dracMoves :: GameState -> [DracTurn]
dracMoves = fmap DracTurn . walks 3 . drac


-- walks start len gives all walks starting at start and of length at
-- most len. The length is the number of rooms in the walk, which does
-- not include the starting room (so the empty walk is valid)
walks :: Int -> Room -> [[Room]]
walks 0 start   = [[]] -- ONLY valid walk of length 0
walks len start = [] : do
    start' <- adjacent start
    (:) start' <$> walks (len - 1) start'


-- All the rooms that are AT MOST n away from the starting room
roomCloud :: Int -> Room -> [Room]
roomCloud 0 start = [start]
roomCloud n start = start : filter (/= start) (adjacent start >>= roomCloud (n - 1))


actionPartition :: [([Int], [Int])]
actionPartition = twoParts [0..3] -- Indexes into the vector
    where
        -- Find all ORDERED two-partitions of the input list
        twoParts :: Eq a => [a] -> [([a], [a])]
        twoParts []     = [([], [])]
        twoParts (x:xs) = let prev = twoParts xs in
            (first (x :) <$> prev) ++ (second (x :) <$> prev)


{-
 - Minimax algorithm implementation using mutually recursive functions optMin
 - and optMax
 -
 - TODO: alpha-beta pruning
 -}

-- Dracula's Turn
optMax :: Int -> GameState -> (Int, DracTurn)
optMax depth startState = undefined


-- Players' Turn
optMin :: Int -> GameState -> (Int, PlayerTurn)
optMin depth startState = undefined
