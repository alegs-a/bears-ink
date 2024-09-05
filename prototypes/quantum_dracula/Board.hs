module Board(Room(..), adjacenyList, adjacent) where


-- All rooms on the map
data Room = CryptAL | HallN | CryptBR | Alley | BonePit | Tomb | Way
          | GrandEntrance | Gallery | EntranceHall | Closet | Dungeon
          | Vent | Dining | Ballroom | Aiden | Library | CryptBL | Chapel
          | Passage | Nest | Bathroom | Canal | Staircase | CryptAR | HallS
          deriving (Eq, Show)


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
