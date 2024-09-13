module Game(Sunlight(..), GameState(..), Room(..), adjacent, allPositions, bitePositions) where

import Data.Either (isRight)
import Test.QuickCheck.Arbitrary


data Sunlight = Sunlight
    { castFrom :: Room
    , castTo :: Room
    }

data GameState = GameState -- all of the game state that dracula cares about
    { positions :: [Either Room Room] -- Left if cannot be bitten, Right if can be bitten
    , sunlights :: [Sunlight]
    , canBite :: Bool
    , lastBite :: Int -- the number of rounds since the last bite
    , lastInfo :: Int -- the number of rounds since players recieved some
    }


bitePositions :: GameState -> [Room]
bitePositions = map proj . filter isRight . positions


allPositions :: GameState -> [Room]
allPositions = map proj . positions


proj :: Either a a -> a
proj (Left x) = x
proj (Right x) = x


-- All rooms on the map
data Room = NHall | Tomb | GuardedWay | Gallery | Alley | BonePit | Entrance
          | Vent | Dungeon | Dining | Library | Crypt | Passage | Chapel | Nest
          | Bathroom | Canal | Staircase | Cellar | SHall | Ballroom
          deriving (Eq, Show, Enum)

instance Arbitrary Room where
    arbitrary = toEnum . (`mod` 21) <$> arbitrary


-- undirected graph of rooms
adjacenyList :: [(Room, Room)]
adjacenyList =
    [ (NHall, Tomb)
    , (NHall, Entrance)
    , (Tomb, GuardedWay)
    , (Tomb, BonePit)
    , (BonePit, Alley)
    , (Alley, Gallery)
    , (Alley, Dungeon)
    , (Gallery, Ballroom)
    , (GuardedWay, Gallery)
    , (Entrance, Vent)
    , (Entrance, Library)
    , (Vent, Dungeon)
    , (Vent, Crypt)
    , (Dungeon, Dining)
    , (Dungeon, Bathroom)
    , (Dining, Ballroom)
    , (Library, Passage)
    , (Crypt, Chapel)
    , (Passage, Chapel)
    , (Chapel, Nest)
    , (Nest, Bathroom)
    , (Bathroom, Canal)
    , (Canal, Cellar)
    , (Cellar, Staircase)
    , (Staircase, Ballroom)
    , (Cellar, SHall)
    , (Passage, SHall)
    ]


adjacent :: Room -> [Room]
adjacent r = firsts ++ seconds
    where
        firsts  = snd <$> filter ((== r) . fst) adjacenyList
        seconds = fst <$> filter ((== r) . snd) adjacenyList
