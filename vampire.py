class Room:
    def __init__(self, id, adj, pos, name):
        self.id = id
        self.adj = adj
        self.light = False

        self.name = name
        self.pos = pos

class Player:
    def __init__(self):
        self.health = 2
        self.room = None
        self.light = 5
        self.stake = 5

    def take_damage(self):
        self.health -= 1

    def move(self, room):
        # Can player stay in same room / move 0?
        if room.index in self.room.adj:
            self.room = room
    
    def make_light(self, room):
        if room.index in self.room.adj:
            if self.light > 0:
                self.light -= 1
                room.light = True

class Dracula:
    def __init__(self):
        self.health = 5
        self.room = None
    
    def take_damage(self):
        self.health -= 1
    
    def move(self, room):
        if room.index in self.room.adj:
            self.room = room

class Gamestate:
    def __init__(self, players, layout):
        if players <= 4:
            self.rooms = []
            self.dracula = Dracula()
            self.players = [Player() for _ in range(players)]

            if layout == 0:
                self.rooms.append(Room(1, [1, 2, 4], (0, 0), "R1"))
                self.rooms.append(Room(2, [2, 1, 3], (1, 0), "R2"))
                self.rooms.append(Room(3, [3, 2, 6], (2, 0), "R3"))
                self.rooms.append(Room(4, [4, 1, 7], (0, 1), "R4"))
                self.rooms.append(Room(6, [6, 3, 9], (2, 1), "R6"))
                self.rooms.append(Room(7, [7, 4, 8], (0, 2), "R7"))
                self.rooms.append(Room(8, [8, 7, 9], (1, 2), "R8"))
                self.rooms.append(Room(9, [9, 6, 8], (2, 2), "R9"))
                for player in self.players:
                    player.room = next(x for x in self.rooms if x.name == "R1")
                self.dracula.room =  next(x for x in self.rooms if x.name == "R9")

    def draw(self, show_dracula=False):
        w, h = 0, 0
        for room in self.rooms:
            w = max(room.pos[0], w)
            h = max(room.pos[1], h)
        w = (w + 1) * 5
        h = (h + 1) * 4
        grid = [[" "] * w for _ in range(h)]

        for room in self.rooms:
            x, y = room.pos 
            x *= 5
            y *= 4
            for i in range(5):
                grid[y][x + i] = "#"
                grid[y + 3][x + i] = "#"
            for i in range(2):
                grid[y + 1 + i][x] = "#"
                grid[y + 1 + i][x + 4] = "#"
            if room.light:
                grid[y + 2][x + 3] = "S"
        
        player_pos = {0:(0, 0), 1:(1, 0), 2:(0, 1), 3:(1, 1)}
        for i in range(len(self.players)):
            x, y = self.players[i].room.pos 
            x *= 5
            y *= 4
            d_x, d_y = player_pos[i]
            grid[y + 1 + d_y][x + 1 + d_x] = str(i + 1)
        
        if show_dracula:
            x, y = self.dracula.room.pos 
            x *= 5
            y *= 4
            grid[y + 1][x + 3] = "D"

        print("\n".join(["".join(line) for line in grid]))

    def player_turn(self):
        for i in range(len(self.players)):
            while True:
                command = input(f"Player {i + 1}:")
                self.draw(True)

gamestate = Gamestate(2, 0)
gamestate.draw(True)
gamestate.player_turn()