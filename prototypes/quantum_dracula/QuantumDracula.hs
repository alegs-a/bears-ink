{-# LANGUAGE LambdaCase #-}

module QuantumDracula
    ( DraculaState
    , draculaTurn
    , isPresent
    , turnStarts
    , bite
    , shortest
    , walkEnds
    , unique
    , passiveness
    , draculaMoves
    ) where

import Game

import Control.Monad.Trans.State.Lazy
import System.Random
import Control.Monad.Trans.Class (lift)
import Control.Monad (when, void)
import Data.List ((\\), maximumBy)
import Data.Bifunctor (bimap)
import Data.Ord (Down(..))
import Data.Function (on)


type DraculaState = StateT [Room] IO


-- soft maximums. used for probabilities
withoutBite,withoutInfo :: Int
withoutBite = 5
withoutInfo = 4

-- The number of moves Dracula can make on his turn
draculaMoves :: Int
draculaMoves = 3

-- How passive should Dracula be?
passiveness :: Float
passiveness = 1


-- Dracula either:
--  bites, and collapses the possible rooms into a much smaller set, or
--  just updates the rooms
draculaTurn :: GameState -> DraculaState [Room]
draculaTurn st = do
    dist <- get
    threshold <- ((fromIntegral (lastBite st) / fromIntegral withoutBite) *) <$> lift stdUnif
    lift (bite st dist) >>= \case
        Just (score, bites, endRooms) | score <= threshold -> do
            put endRooms
            return . unique $ bites -- don't count for multiplicity here
        _noBitesOrLowScore -> do
            let (starting, moves) = turnStarts st dist
            let inaccessible = bitePositions st ++ (castTo <$> sunlights st)
            -- Don't update dist if Dracula is in sunligh and CANNOT move
            when (moves /= 0) . void . put $ walkEnds inaccessible moves (filter (`notElem` inaccessible) starting)
            return []


-- Compute the starting distribution for Dracula on his turn and the number of
-- moves he has remaining to take.
-- If Dracula is in a room with sunlight, make turnStarts the possible rooms
-- Dracula could start in AFTER moving out of the sunlight room.
turnStarts :: GameState -> [Room] -> ([Room], Int)
turnStarts st dist = case dist of
    -- Case for in a room with sunlight
    [room] | not . null . dropWhile (/= room) $ to ->
        if null validRooms then ([room], 0) else (validRooms, draculaMoves - 1)
        where
            to = castTo <$> sunlights st
            -- The room the sunlight was cast from
            from = castFrom . head . dropWhile ((/= room) . castTo) $ sunlights st
            -- Valid moves to take to escape sunlight
            validRooms = filter (`notElem` (from : to)) $ walkEnds [room] 1 []
    _notInSunlight -> (dist, draculaMoves)


-- Get the scores for each potential bite, randomly weighting all but the
-- opportunity to bite more than one player. If no bites are possible, give back
-- Nothing. Otherwise, give back the (randomly weighted) score, the rooms to
-- bite in, and the list of good rooms to end the turn in
bite :: GameState -> [Room] -> IO (Maybe (Float, [Room], [Room]))
bite st dist = do
    scored <- traverse score . filter (not . null . fst) $ possibleBites
    case scored of
        [] -> pure Nothing
        xs -> pure . Just . maximumBy comp $ xs
    where
        -- bites that are actually possible
        possibleBites = bestBite numMoves starting [] <$> playerRooms
        (starting, numMoves) = turnStarts st dist
        playerRooms = unique . bitePositions $ st
        sunlightRooms = castTo <$> sunlights st

        -- return the pair with lex in order snd, reverse order on fst
        comp :: (Float, [Room], [Room]) -> (Float, [Room], [Room]) -> Ordering
        comp (s1, r1, _) (s2, r2, _) = case on compare length r1 r2 of
            EQ -> on compare Down s1 s2
            ord -> ord

        -- 0 if more than one bite, otherwise rand * 1/(number of safe rooms)
        -- (say 1/0 = 1)
        score :: ([Room], [Room]) -> IO (Float, [Room], [Room])
        score (biteSeq, safe)
            | length biteSeq > 1 = pure (0, biteSeq, safe)
            | otherwise = stdUnif
                >>= (pure . (, biteSeq, safe))
                    . (passiveness/(fromIntegral . length) safe *)


        -- Given a target room to do the next bite in, return the best sequence
        -- of ALL rooms to bite in during the turn (counted with multiplicity)
        -- and the safe rooms to end the turn in
        -- NOTE: the best bite is given by lex order on
        --  (number of players bitten, number of "safe" rooms)
        -- where a room is called "safe" it is at least one space away from any
        -- player who does not miss their next turn
        bestBite :: Int -> [Room] -> [Room] -> Room -> ([Room], [Room])
        bestBite moves starts alreadyBitten biteIn
            | remaining < 0 = ([], []) -- due to lex order, only need to have last entry empty
            | otherwise = maximumBy (compare `on` bimap length length)
                $ (bitten, safeRooms) : (bestBite remaining [biteIn] bitten <$> notBitten)
            where
                -- number of moves after this bite
                remaining = moves - len
                len = shortest sunlightRooms biteIn starts
                -- bites so far, counted with multiplicity
                bitten = multiList ++ alreadyBitten
                multiList = replicate (length . filter (== biteIn) . bitePositions $ st) biteIn
                -- all players not yet bitten
                notBitten = allPositions st \\ bitten
                -- "safe" rooms
                safeRooms = foldl (\\) safe
                        . map (walkEnds [] 1)
                        . pure
                    $ notBitten
                safe = walkEnds sunlightRooms remaining [biteIn]


-- Find the length of the shortest path from dist to end.
-- If no path exists, or the length of the path is greater than draculaMoves,
-- return draculaMoves + 1
-- NOTE: tail recursive => able to terminate if no path exists
shortest :: [Room] -> Room -> [Room] -> Int
shortest inaccessible end = shortest' 0
    where
        shortest' :: Int -> [Room] -> Int
        shortest' len dist = if len > draculaMoves || end `elem` dist
            then len
            else shortest' (len + 1) (walkEnds inaccessible 1 dist)


-- Necessary properties:
--  o If Dracula could only possible be in the room that is checked, return true
--  o If Dracula could not possibly be in the room that is checked, return false
--  o The closer lastBite is to withoutBite, the more likely Dracula is to be
--    within range of biting some player (not necessarily the one checking)
-- NOTE: assume that Dracula doesn't care whether an information request is via
-- garlic or sunlight
isPresent :: GameState -> Room -> DraculaState Bool
isPresent st room = do
    dist <- get
    if room `notElem` dist then return False else do
        infoRoll <- lift stdUnif
        biteRoll <- lift stdUnif
        let infoThreshold = fromIntegral (lastInfo st) / fromIntegral (length dist * withoutInfo)
        let biteThreshold = fromIntegral (lastBite st) / fromIntegral withoutBite
        let res = infoRoll <= infoThreshold && biteRoll <= biteThreshold
        if res then put [room] else modify $ filter (/= room)
        return res


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
