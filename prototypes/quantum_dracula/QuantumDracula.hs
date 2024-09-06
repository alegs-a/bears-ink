{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE LambdaCase #-}

module QuantumDracula(DraculaState(..), draculaTurn, isPresent) where

import Control.Monad.Trans.State.Lazy
import System.Random
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)

import Game


type DraculaState = StateT [Room] IO


-- soft maximums. used for probabilities
withoutBite,withoutInfo :: Int
withoutBite = 5
withoutInfo = 4


-- Dracula either:
--  bites, and collapses the possible rooms into a much smaller set, or
--  just updates the rooms
draculaTurn :: GameState -> DraculaState [Room]
draculaTurn st = do
    dist <- get
    threshold <- (/fromIntegral withoutBite) <$> lift stdUnif
    lift (bestBite st dist) >>= \case
        Just (bites, score, endRooms) | score <= threshold -> do
            put endRooms
            return bites
        _noBitesOrLowScore -> do
            let (starting, moves) = turnStarts st dist
            let inaccessible = bitePositions st ++ (castTo <$> sunlights st)
            put $ walkEnds inaccessible moves (filter (`notElem` inaccessible) starting)
            return []


-- get the starting rooms for Dracula on his turn
-- If Dracula is in a room with sunlight, make turnStarts the possible rooms
-- Dracula could start in AFTER moving out of the sunlight room.
-- To make this work, additionally return the number of turns Dracula has left
-- (2 or 3 or 0)
turnStarts :: GameState -> [Room] -> ([Room], Int)
turnStarts st dist = case dist of
    [room] | not . null . dropWhile ((/= room) . castTo) $ sunlights st ->
        let
            from = castFrom . head . dropWhile ((/= room) . castTo) $ sunlights st
            rooms = filter (`notElem` (castTo <$> sunlights st)) $ walkEnds [room] 1 []
        in
            if null rooms then ([room], 0) else (rooms, 2)
    _notInSunlight -> (dist, 3)


-- Get the scores for each potential bite, randomly weighting all but the
-- opportunity to bite more than one player. If no bites are possible, give back
-- Nothing. Otherwise, give back the rooms to bite in, the (randomly weighted)
-- score, and the list of good rooms to end the turn in
bestBite :: GameState -> [Room] -> IO (Maybe ([Room], Float, [Room]))
bestBite st dist = undefined
    where
        inaccessible = bitePositions st ++ (castTo <$> sunlights st)


-- Find the length of the shortest path from dist to end.
shortest :: [Room] -> [Room] -> Room -> Int
shortest inaccessible dist end
    | elem end dist = 0
    | otherwise = 1 + shortest inaccessible (walkEnds inaccessible 1 dist) end


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
        let info = fromIntegral (lastInfo st) / fromIntegral (length dist)
        let bite = fromIntegral (lastBite st) / fromIntegral withoutBite
        let res = infoRoll <= info && biteRoll <= bite
        when res $ put [room]
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
