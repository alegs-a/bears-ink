class Room:
    def __init__(self, index):
        self.index = index
        self.adj = []
        self.light = False

    def make_light(self):
        self.light = True

class Player:
    def __init__(self):
        self.health = 2
        self.room = None
        self.light = 5
        self.stake = 5

    def take_damage(self):
        self.health -= 1

    def move(self, room):
        if room.index in self.room.adj:
            self.room = room
    
    def make_light(self, room):
        if room.index in self.room.adj:
            if self.light > 0:
                self.light -= 1
                room.make_light()

class Dracula:
    def __init__(self):
        self.health = 5
        self.room = None
    
    def take_damage(self):
        self.health -= 1
    
    def move(self, room):
        if room.index in self.room.adj:
            self.room = room

