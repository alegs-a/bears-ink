module Game where -- export everything

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

import Board


-- Player inventories
data Inventory = Inventory { stakes :: Int, lights :: Int } deriving Show


-- NOTE: Assume that a garlic cloud is sufficient (otherwise it is just too many
-- options)
data PlayerTurn = PlayerTurn
    { moves :: Vector Room
    , garlicCloud :: [Room]
    , inventoryChanges :: Vector Inventory
    }

data GameState = GameState
    { positions :: Vector Room
    , inventories :: Vector Inventory
    , sunlights :: Vector Room
    , playerLives :: Int
    , dracLives :: Int
    , canBite :: Bool
    , lastBite :: Int -- the number of turns since the last bite
    , lastInfo :: Int -- the number of turns since players recieved some positive information on Dracula's position
    }


instance Semigroup Inventory where
    i1 <> i2 = Inventory
        {stakes = stakes i1 + stakes i2
        , lights = lights i1 + lights i2}

instance Monoid Inventory where
    mempty = Inventory {stakes = 0, lights=0}
