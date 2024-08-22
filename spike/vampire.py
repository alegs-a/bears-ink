import os
from consts import *

class Room:
    def __init__(self, id, adj, pos, name, size, openings):
        self.id = id
        self.adj = adj
        self.light = False
        self.lightsource = None
        self.pos = pos
        self.name = name
        self.size = size
        self.openings = openings

class PlayerStats:
    def __init__(self):
        self.health = PLAYER_HEALTH
        self.garlic = PLAYER_GARLIC

class Player:
    def __init__(self, id):
        self.id = id
        self.room = None
        self.light = PLAYER_LIGHT
        self.stakes = PLAYER_STAKES
        self.damaged = False
        self.rest = False
    def damage(self):
        self.damaged = True
        if self.stakes > 0:
            self.stakes -= 1

    def move(self, room):
        if not room.id in self.room.adj:
            print("That room is not adjacent.")
            return False
           
        self.room = room
        return True
       
    def garlic(self, room, dracula):
        if not room.id in self.room.adj:
            print("That room is not adjacent.")
            return False
            
        if room.id == dracula.room.id:
            print("Dracula is in that room.")
        else:
            print("Dracula is not in that room.")
        return True
        
    def throw_stake(self, room, dracula):
        if not room.id in self.room.adj:
            print("That room is not adjacent.")
            return False
        if self.stakes <= 0:
            print("You have no more stakes.")
            return False
        
        self.stakes -= 1
        if room.id == dracula.room.id:
            dracula.damage()
        print("You threw a stake.")
        if room.id == dracula.room.id:
            print("Dracula is in that room.")
        else:
            print("Dracula is not in that room.")
        return True
    
        
    def make_light(self, room, dracula):
        if not room.id in self.room.adj:
            print("That room is not adjacent.")
            return False
        if self.light <= 0:
            print("You can't make anymore light.")
            return False
        
        self.light -= 1
        room.light = True
        room.lightsource = self.room.id
        print("You created light.")
        if room.id == dracula.room.id:
            print("Dracula is in that room.")
        else:
            print("Dracula is not in that room.")
        return True
    
class Dracula:
    def __init__(self):
        self.health = DRACULA_HEALTH
        self.room = None
        self.hurt = False
    
    def damage(self):
        self.health -= PLAYER_DAMAGE
        self.hurt = True
    
    def move(self, room, players, playerstats):
        if not room.id in self.room.adj:
            print("That room is not adjacent.")
            return False
        if room.light:
            print("That room contains light.")
            return False
        if self.room.light and room.id == self.room.lightsource:
            print("That room is the source of the light.")
            return False
        
        self.room = room
        for player in players:
            if player.room.id == room.id and not player.damaged and not player.rest and not self.hurt:
                player.damage()
                playerstats.health -= DRACULA_DAMAGE

        return True

class Gamestate:
    def __init__(self, players, layout):
        self.rooms = []
        self.dracula = Dracula()
        self.playerstats = PlayerStats()
        self.players = [Player(i + 1) for i in range(players)]

        if layout == 0:
            self.rooms.append(Room(1, [1, 2, 8], (0, 0), "r01", (7, 1), [[], [], [], [0, 6]]))
            self.rooms.append(Room(2, [2, 1, 3, 9], (0, 1), "r02", (1, 1), [[0], [0], [], [0]]))
            self.rooms.append(Room(3, [3, 2, 4], (1, 1), "r03", (1, 1), [[0], [], [0], []]))
            self.rooms.append(Room(4, [4, 3, 5], (2, 1), "r04", (1, 1), [[0], [], [0], []]))
            self.rooms.append(Room(5, [5, 4, 6, 13], (3, 1), "r05", (1, 2), [[0], [], [0], [0]]))
            self.rooms.append(Room(6, [6, 5, 7], (4, 1), "r06", (1, 1), [[0], [], [0], []]))
            self.rooms.append(Room(7, [7, 6, 8], (5, 1), "r07", (1, 1), [[0], [], [0], []]))
            self.rooms.append(Room(8, [8, 1, 10], (6, 1), "r08", (1, 1), [[], [0], [0], [0]]))
            self.rooms.append(Room(9, [9, 2, 11], (0, 2), "r09", (1, 1), [[], [0], [], [0]]))
            self.rooms.append(Room(10, [10, 8, 16], (6, 2), "r10", (1, 1), [[], [0], [], [0]]))         
            self.rooms.append(Room(11, [11, 9, 12, 17], (0, 3), "r11", (1, 1), [[0], [0], [], [0]]))
            self.rooms.append(Room(12, [12, 11, 18, 13], (1, 3), "r12", (1, 1), [[0], [], [0], [0]]))
            self.rooms.append(Room(13, [13, 12, 5, 14, 19], (2, 3), "r13", (2, 1), [[0], [1], [0], [1]]))
            self.rooms.append(Room(14, [14, 13, 15], (4, 3), "r14", (1, 1), [[0], [], [0], []]))
            self.rooms.append(Room(15, [15, 14, 16], (5, 3), "r15", (1, 1), [[0], [], [0], []]))
            self.rooms.append(Room(16, [16, 10, 15, 20], (6, 3), "r16", (1, 1), [[], [0], [0], [0]]))
            self.rooms.append(Room(17, [17, 11, 21], (0, 4), "r17", (1, 1), [[], [0], [], [0]]))
            self.rooms.append(Room(18, [18, 12, 22], (1, 4), "r18", (1, 1), [[], [0], [], [0]]))
            self.rooms.append(Room(19, [19, 13, 23, 24], (3, 4), "r19", (1, 2), [[1], [0], [1], []]))
            self.rooms.append(Room(20, [20, 16, 25], (6, 4), "r20", (1, 1), [[], [0], [], [0]]))   
            self.rooms.append(Room(21, [21, 17, 22, 26], (0, 5), "r21", (1, 1), [[0], [0], [], [0]]))
            self.rooms.append(Room(22, [22, 18, 21, 23], (1, 5), "r22", (1, 1), [[0], [0], [0], []]))
            self.rooms.append(Room(23, [23, 22, 19], (2, 5), "r23", (1, 1), [[0], [], [0], []]))
            self.rooms.append(Room(24, [24, 19, 25], (4, 5), "r24", (2, 1), [[0], [], [0], []]))
            self.rooms.append(Room(25, [25, 20, 24, 26], (6, 5), "r25", (1, 1), [[], [0], [0], [0]]))
            self.rooms.append(Room(26, [26, 21, 25], (0, 6), "r26", (7, 1), [[], [0, 6], [], []]))
            
            starting = ["r02", "r08", "r21", "r25"]
            for i in range(len(self.players)):
                self.players[i].room = self.find_room(starting[i])
            self.dracula.room = self.find_room("r13")

    def find_room(self, name):
        matches = [x for x in self.rooms if x.name == name]
        if len(matches) == 0:
            return None
        return matches[0]

    def draw_map(self, show_dracula=False):
        w, h = 0, 0
        for room in self.rooms:
            w = max(room.pos[0] + room.size[0], w)
            h = max(room.pos[1] + room.size[1], h)
        w = (w + 1) * ROOM_WIDTH
        h = (h ) * ROOM_HEIGHT
        grid = [[EMPTY_CHAR] * w for _ in range(h)]

        for room in self.rooms:
            x, y = room.pos 
            x *= ROOM_WIDTH
            y *= ROOM_HEIGHT

            for i in range(ROOM_WIDTH * room.size[0]):
                grid[y][x + i] = WALL_CHAR
                grid[y + ROOM_HEIGHT * room.size[1] - 1][x + i] = WALL_CHAR
            for i in range(ROOM_HEIGHT * room.size[1] - 2):
                grid[y + 1 + i][x] = WALL_CHAR
                grid[y + 1 + i][x + ROOM_WIDTH * room.size[0] - 1] = WALL_CHAR
            if room.light:
                grid[y + 2][x + 3] = LIGHT_CHAR
            
            for i in room.openings[0]:
                grid[y + 2 + i * ROOM_HEIGHT][x + ROOM_WIDTH * room.size[0] - 1] = EMPTY_CHAR
            for i in room.openings[2]:
                grid[y + 2 + i * ROOM_HEIGHT][x] = EMPTY_CHAR
            
            for i in room.openings[1]:
               grid[y][x + 3 + i * ROOM_WIDTH] = EMPTY_CHAR
            for i in room.openings[3]:
               grid[y + ROOM_HEIGHT * room.size[1] - 1][x + 3 + i * ROOM_WIDTH] = EMPTY_CHAR

            grid[y + 3][x + 1] = room.name[1]
            grid[y + 3][x + 2] = room.name[2]
        
        for i in range(len(self.players)):
            x, y = self.players[i].room.pos 
            x *= ROOM_WIDTH
            y *= ROOM_HEIGHT
            d_x, d_y = PLAYER_OFFSET[i]
            grid[y + 1 + d_y][x + 1 + d_x] = str(self.players[i].id)
        
        if show_dracula:
            x, y = self.dracula.room.pos 
            x *= ROOM_WIDTH
            y *= ROOM_HEIGHT
            grid[y + 1][x + 3] = DRACULA_CHAR

        print("\n".join(["".join(line) for line in grid]))
    
    def draw_stats(self, show_dracula=False):
        names = ["Stats"]
        light = ["Light"]
        stakes = ["Stakes"]
        for i in range(len(self.players)):
            names.append(f"P{self.players[i].id}")
            light.append(str(self.players[i].light))
            stakes.append(str(self.players[i].stakes))

        if show_dracula:
            print("Drac HP: " + str(self.dracula.health))
        print("Hunters HP: " + str(self.playerstats.health) + "\tGarlic: " + str(self.playerstats.garlic))

        print("\t".join(names))
        print("\t".join(light))
        print("\t".join(stakes))
        print()

    def player_turn(self):
        if DRACULA_MANUAL:
            os.system('cls')
        self.playerstats.garlic = PLAYER_GARLIC
        for i in range(len(self.players) - 1, -1, -1):
            if self.players[i].damaged:
                print(f"Player {self.players[i].id} was bitten.")
        
        for i in range(len(self.players)):
            if self.players[i].rest:
                self.players[i].rest = False
            if self.players[i].damaged:
                self.players[i].damaged = False
                self.players[i].rest = True
                continue

            self.draw_map(PLAYER_GODMODE)
            self.draw_stats(PLAYER_GODMODE)

            moved = False
            garlic = False

            while True:
                command = None
                if PLAYER_MANUAL:
                    command = input(f"Player {self.players[i].id}: ").split(" ", 1)
                else:
                    command = ai_player_to_command(ai_player(self.players, i, len(self.rooms)), len(self.rooms)).split(" ", 1)
                if len(command) == 1:
                    command += ['']
                action, arg = command

                if action == "help":
                    print("map                View the map.")
                    print("stats              View stats of the current players.")
                    print("rest [light|stake] Rest to gain a resource.")
                    print("mv [room]          Move into an adjacent room.")
                    print("gar [room]         Throw garlic to check if Dracula is in an adjecent room.")
                    print("light [room]       Create light in an adjacent room to stop Dracula entering.")
                    print("stake [room]       Throw a stake into an adjacent room to attack Dracula.")
                    print("skip               End your turn.")
                elif action == "map":
                    self.draw_map(PLAYER_GODMODE)
                elif action == "stats":
                    self.draw_stats(PLAYER_GODMODE)
                elif action == "rest":
                    if garlic or moved:
                        print("You have already done an action on this turn.")
                        continue
                    if arg == "light":
                        self.players[i].light += 1
                        if self.players[i].light > MAX_LIGHT:
                            self.players[i].light = MAX_LIGHT
                        break
                    elif arg == "stake":
                        self.players[i].stakes += 1
                        if self.players[i].stakes > MAX_STAKES:
                            self.players[i].stakes = MAX_STAKES
                        break
                    else:
                        print("That resource does not exist.")

                elif action == "mv":
                    if moved:
                        print("You have already moved on your turn.")
                        continue
                    room = self.find_room(arg)
                    if room == None:
                        print("That room does not exist.")
                        continue
                    moved = self.players[i].move(room)
                elif action == "gar":
                    if self.playerstats.garlic <= 0:
                        print("You have no garlic left.")
                        continue
                    room = self.find_room(arg)
                    if room == None:
                        print("That room does not exist.")
                        continue
                    garlic = self.players[i].garlic(room, self.dracula)
                    self.playerstats.garlic -= 1
                elif action == "light":
                    room = self.find_room(arg)
                    if room == None:
                        print("That room does not exist.")
                        continue
                    if not self.players[i].make_light(room, self.dracula):
                        continue
                    break
                elif action == "stake":
                    room = self.find_room(arg)
                    if room == None:
                        print("That room does not exist.")
                        continue
                    if not self.players[i].throw_stake(room, self.dracula):
                        continue
                    if self.dracula.health == 0:
                        return True
                    break
                elif action == "skip":
                    break
                else:
                    print("Unknown command")
        return False

    def dracula_turn(self):
        if PLAYER_MANUAL:
            os.system('cls')
        if len(self.players) == 0:
            return True
        if DRACULA_MANUAL:
            self.draw_map(True)
            self.draw_stats(True)
        moves = 0
        while True:
            command = None
            if DRACULA_MANUAL:
                command = input("Dracula: ").split(" ", 1)
            else:
                command = ai_dracula_to_command(ai_dracula(self.dracula, self.players, len(self.rooms))).split(" ", 1)
            if len(command) == 1:
                command += ['']
            action, arg = command

            if action == "help":
                print("map       View the map.")
                print("stats     View stats of the current players.")
                print("mv [room] Move into an adjacent room.")
                print("skip      End your turn.")
            elif action == "map":
                self.draw_map(True)
            elif action == "stats":
                self.draw_stats(True)
            elif action == "mv":
                room = self.find_room(arg)
                if room == None:
                    print("That room does not exist.")
                    continue
                moved = self.dracula.move(room, self.players, self.playerstats)
                if moved:
                    moves += 1
                    if moves == DRACULA_MOVES:
                         break
            elif action == "skip":
                break
            else:
                print("Unknown command")

        for room in self.rooms:
            room.light = False
        if self.dracula.hurt:
            self.dracula.hurt = False
        return False

def main():
    gamestate = Gamestate(PLAYER_COUNT, LAYOUT)
    while True:
        if gamestate.player_turn():
            print("Dracula has been defeated. Players win!")
            break
        if gamestate.dracula_turn():
            print("All players have been killed. Dracula wins!")
            break
    while True:
        pass

def ai_dracula_to_command(code):
    room = str(code + 1)
    if len(room) == 1:
        room = '0' + room
    return "mv r" + room

def ai_player_to_command(code, n):
    room = code % n
    action = code // n
    if action == 4:
        if room == 0:
            return "rest light"
        elif room == 1:
            return "rest stake"
        elif room == 2:
            return "skip"
    
    room = str(room + 1)
    if len(room) == 1:
        room = '0' + room
    if action == 0:
        return "mv r" + room
    elif action == 1:
        return "gar r" + room
    elif action == 2:
        return "light" + room
    elif action == 3:
        return "stake r" + room

def ai_dracula(dracula, players, n):
    return dracula.room.id - 1

def ai_player(players, i, n):
    return 106

if __name__ == "__main__":
    main()