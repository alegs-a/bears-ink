module Game where -- export everything

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector


-- Player inventories
data Inventory = Inventory { stakes :: Int, lights :: Int } deriving Show

-- All rooms on the map
data Room = CryptAL | HallN | CryptBR | Alley | BonePit | Tomb | Way
          | GrandEntrance | Gallery | EntranceHall | Closet | Dungeon
          | Vent | Dining | Ballroom | Aiden | Library | CryptBL | Chapel
          | Passage | Nest | Bathroom | Canal | Staircase | CryptAR | HallS
          deriving (Eq, Show)

-- just a list of rooms to move through, with length at most 3
newtype DracTurn = DracTurn [Room]

-- NOTE: Assume that a garlic cloud is sufficient (otherwise it is just too many
-- options)
data PlayerTurn = PlayerTurn
    { moves :: Vector Room
    , garlicCloud :: [Room]
    , inventoryChanges :: Vector Inventory
    }

data GameState = GameState
    { drac :: Room
    , positions :: Vector Room
    , inventories :: Vector Inventory
    , playerLives :: Int
    , dracLives :: Int
    , lastKnownLocation :: Maybe Room -- if the players knew Drac's position last turn
    , canBite :: Bool
    }


instance Semigroup Inventory where
    i1 <> i2 = Inventory
        {stakes = stakes i1 + stakes i2
        , lights = lights i1 + lights i2}

instance Monoid Inventory where
    mempty = Inventory {stakes = 0, lights=0}


-- undirected graph of rooms
adjacenyList :: [(Room, Room)]
adjacenyList =
    [ (CryptAL, HallN)
    , (HallN, CryptBR)
    , (CryptAL, Alley)
    , (Alley, BonePit)
    , (BonePit, Tomb)
    , (Tomb, Way)
    , (GrandEntrance, CryptBR)
    , (CryptAL, Gallery)
    , (Gallery, EntranceHall)
    , (EntranceHall, Closet)
    , (Closet, Dungeon)
    , (Closet, Passage)
    , (Dungeon, Tomb)
    , (Dungeon, Vent)
    , (Dungeon, Bathroom)
    , (Vent, Dining)
    , (Dining, Ballroom)
    , (Ballroom, Aiden)
    , (Ballroom, Staircase)
    , (Aiden, CryptBR)
    , (CryptBL, Chapel)
    , (CryptBL, HallS)
    , (Passage, Chapel)
    , (Chapel, Nest)
    , (Bathroom, Nest)
    , (Bathroom, Canal)
    , (Canal, CryptAR)
    , (HallS, CryptAR)
    , (CryptAR, Staircase)
    ]


adjacent :: Room -> [Room]
adjacent r = firsts ++ seconds
    where
        firsts  = snd <$> filter ((== r) . fst) adjacenyList
        seconds = fst <$> filter ((== r) . snd) adjacenyList
