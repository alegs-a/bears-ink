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
        self.stakes = 5

    def damage(self):
        self.health -= 1

    def move(self, room):
        # Can player stay in same room / move 0?
        if room.id in self.room.adj:
            self.room = room
            return True
        else:
            print("That room is not adjacent.")
            return False
       
    def garlic(self, room, dracula):
        if room.id in self.room.adj:
            if room.id == dracula.room.id:
                print("Dracula is in that room.")
            else:
                print("Dracula is not in that room.")
            return True
        else:
            print("That room is not adjacent.")
            return False
        
    def throw_stake(self, room, dracula):
        if room.id in self.room.adj:
            if self.stakes > 0:
                self.stakes -= 1
                if room.id == dracula.room.id:
                    dracula.damage()
                print("You threw a stake.")
                return True
            else:
                print("You have no more stakes.")
                return False
        else:
            print("That room is not adjacent.")
            return False
        
    def make_light(self, room):
        if room.id in self.room.adj:
            if self.light > 0:
                self.light -= 1
                room.light = True
                print("You created light.")
                return True
            else:
                print("You can't make anymore light.")
                return False
        else:
            print("That room is not adjacent.")
            return False

class Dracula:
    def __init__(self):
        self.health = 3
        self.room = None
    
    def damage(self):
        self.health -= 1
    
    def move(self, room):
        if room.id in self.room.adj:
            self.room = room

class Gamestate:
    def __init__(self, players, layout):
        if players <= 4:
            self.rooms = []
            self.dracula = Dracula()
            self.players = [Player() for _ in range(players)]

            if layout == 0:
                self.rooms.append(Room(1, [1, 2, 4], (0, 0), "r1"))
                self.rooms.append(Room(2, [2, 1, 3], (1, 0), "r2"))
                self.rooms.append(Room(3, [3, 2, 6], (2, 0), "r3"))
                self.rooms.append(Room(4, [4, 1, 7], (0, 1), "r4"))
                self.rooms.append(Room(6, [6, 3, 9], (2, 1), "r6"))
                self.rooms.append(Room(7, [7, 4, 8], (0, 2), "r7"))
                self.rooms.append(Room(8, [8, 7, 9], (1, 2), "r8"))
                self.rooms.append(Room(9, [9, 6, 8], (2, 2), "r9"))

                for player in self.players:
                    player.room = self.find_room("r1")
                self.dracula.room = self.find_room("r9")

    def find_room(self, name):
        matches = [x for x in self.rooms if x.name == name]
        if len(matches) == 0:
            return None
        return matches[0]

    def draw_map(self, show_dracula=False):
        w, h = 0, 0
        for room in self.rooms:
            w = max(room.pos[0], w)
            h = max(room.pos[1], h)
        w = (w + 1) * 6
        h = (h + 1) * 4
        grid = [[" "] * w for _ in range(h)]

        for room in self.rooms:
            x, y = room.pos 
            x *= 6
            y *= 4
            for i in range(6):
                grid[y][x + i] = "#"
                grid[y + 3][x + i] = "#"
            for i in range(2):
                grid[y + 1 + i][x] = "#"
                grid[y + 1 + i][x + 5] = "#"
            if room.light:
                grid[y + 2][x + 3] = "S"
            grid[y][x + 2] = room.name[0]
            grid[y][x + 3] = room.name[1]
        
        player_pos = {0:(0, 0), 1:(1, 0), 2:(0, 1), 3:(1, 1)}
        for i in range(len(self.players)):
            x, y = self.players[i].room.pos 
            x *= 6
            y *= 4
            d_x, d_y = player_pos[i]
            grid[y + 1 + d_y][x + 1 + d_x] = str(i + 1)
        
        if show_dracula:
            x, y = self.dracula.room.pos 
            x *= 6
            y *= 4
            grid[y + 1][x + 3] = "D"

        print("\n".join(["".join(line) for line in grid]) + "\n")
    
    def draw_stats(self, show_dracula=False):
        names = ["Stats"]
        health = ["Health"]
        light = ["Light"]
        stakes = ["Stakes"]
        for i in range(len(self.players)):
            names.append(f"P{i + 1}")
            health.append(str(self.players[i].health))
            light.append(str(self.players[i].light))
            stakes.append(str(self.players[i].stakes))

        if show_dracula:
            names.append("Drac")
            health.append(str(self.dracula.health))

        print("\t".join(names))
        print("\t".join(health))
        print("\t".join(light))
        print("\t".join(stakes))
        print()

    def player_turn(self):
        for i in range(len(self.players)):
            self.draw_map(True)
            self.draw_stats(True)

            moved = False
            garlic = False

            while True:
                command = input(f"Player {i + 1}: ").split(" ", 1)
                if len(command) == 1:
                    command += ['']
                action, arg = command

                if action == "help":
                    print("map           View the map.")
                    print("stats         View stats of the current players.")
                    print("mv     [room] Move into an adjacent room.")
                    print("gar    [room] Throw garlic to check if Dracula is in an adjecent room.")
                    print("light  [room] Create light in an adjacent room to stop Dracula entering.")
                    print("stake  [room] Throw a stake into an adjacent room to attack Dracula.")
                    print("skip          End your turn.")
                elif action == "map":
                    self.draw_map(True)
                elif action == "stats":
                    self.draw_stats(True)
                elif action == "mv":
                    if moved:
                        print("You have already moved on your turn.")
                        continue
                    room = self.find_room(arg)
                    if room == None:
                        print("That room does not exist.")
                        continue
                    moved = self.players[i].move(room)
                    if moved:
                        self.draw_map(True)
                elif action == "gar":
                    if garlic:
                        print("You have already thrown garlic on your turn.")
                        continue
                    room = self.find_room(arg)
                    if room == None:
                        print("That room does not exist.")
                        continue
                    garlic = self.players[i].garlic(room, self.dracula)
                elif action == "light":
                    room = self.find_room(arg)
                    if room == None:
                        print("That room does not exist.")
                        continue
                    if not self.players[i].make_light(room):
                        continue
                    break
                elif action == "stake":
                    room = self.find_room(arg)
                    if room == None:
                        print("That room does not exist.")
                        continue
                    if not self.players[i].throw_stake(room, self.dracula):
                        continue
                    break
                elif action == "skip":
                    break
                else:
                    print("Unknown command")

gamestate = Gamestate(1, 0)
while True:
    gamestate.player_turn()