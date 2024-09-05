{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module QuantumDracula(DraculaState(..), draculaTurn, isPresent) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Control.Monad.Trans.State.Lazy
import System.Random
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)

import Board
import Game (GameState(..))
import Text.ParserCombinators.ReadP (between)


type DraculaState = StateT [Room] IO


-- soft maximums. used for probabilities
withoutBite,withoutInfo :: Int
withoutBite = 5
withoutInfo = 4


-- Dracula either:
--  bites, and collapses the possible rooms into a much smaller set, or
--  just updates the rooms
draculaTurn :: GameState -> DraculaState [Room]
draculaTurn st = undefined -- To bite or not to bite?


-- Necessary properties:
--  o If Dracula could only possible be in the room that is checked, return true
--  o If Dracula could not possibly be in the room that is checked, return false
--  o The closer lastBite is to withoutBite, the more likely Dracula is to be
--    within range of biting some player (not necessarily the one checking)
-- NOTE: assume that Dracula doesn't care whether an information request is via
-- garlic or sunlight
-- NOTE: for now, this is just considers a random number, last seen, and last bite.
isPresent :: GameState -> Room -> DraculaState Bool
isPresent st room = do
    dist <- get
    if notElem room dist then return False else do
        infoRoll <- lift stdUnif
        biteRoll <- lift stdUnif
        let biteEnds = Vector.toList $ positions st
        let biteMoves = 3 * (withoutBite - lastBite st)
        let bitesInaccessible = Vector.toList (sunlights st) ++ Vector.toList (positions st)
        let toBite = walkEnds biteEnds biteMoves bitesInaccessible
        let intersect = intersectSize toBite dist
        let info = fromIntegral (lastInfo st) / fromIntegral (length dist)
        let bite = fromIntegral (lastBite st * intersect) / fromIntegral (withoutBite * length toBite)
        let res = infoRoll <= info && biteRoll <= bite
        when res $ put (filter (/= room) dist)
        return res


-- Compute the size of the intersection of two lists, not counted with
-- multiplicities
intersectSize :: Eq a => [a] -> [a] -> Int
intersectSize [] _ = 0
intersectSize _ [] = 0
intersectSize (x:xs) ls
    | elem x xs = intersectSize xs ls
    | elem x ls = 1 + intersectSize xs (filter (/= x) ls)
    | otherwise = intersectSize xs ls


-- generate a random float in [0,1]
stdUnif :: IO Float
stdUnif = getStdRandom random


-- Find all of the rooms that can be reached with a walk of length at most len
-- that starts in some room in starting. None of the rooms in inaccessible may
-- be used.
-- Assume that starting and inaccessible are disjoint
walkEnds :: [Room] -> Int -> [Room] -> [Room]
walkEnds _ 0 starting = starting
walkEnds inaccessible len starting = walkEnds inaccessible (len - 1) thisStep
    where
        accessible = flip notElem inaccessible
        -- if there are at least two elements of starting, it will be included
        -- in the adjacent
        thisStep = case starting of
            [x] -> x : filter accessible (adjacent x) -- already unique
            xs  -> unique . filter accessible $ xs >>= adjacent


unique :: Eq a => [a] -> [a]
unique = foldr (\x -> (x:) . filter (/= x)) []
