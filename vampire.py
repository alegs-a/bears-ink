import os
from consts import *

class Room:
    def __init__(self, id, adj, pos, name):
        self.id = id
        self.adj = adj
        self.light = False

        self.name = name
        self.pos = pos

class Player:
    def __init__(self, id):
        self.id = id
        self.health = PLAYER_HEALTH
        self.room = None
        self.light = PLAYER_LIGHT
        self.stakes = PLAYER_STAKES
        self.damaged = False

    def damage(self):
        self.health -= DRACULA_DAMAGE
        self.damaged = True

    def move(self, room):
        # Can player stay in same room / move 0?
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
        return True
    
        
    def make_light(self, room):
        if not room.id in self.room.adj:
            print("That room is not adjacent.")
            return False
        if self.light <= 0:
            print("You can't make anymore light.")
            return False
        
        self.light -= 1
        room.light = True
        print("You created light.")
        return True
    
class Dracula:
    def __init__(self):
        self.health = DRACULA_HEALTH
        self.room = None
    
    def damage(self):
        self.health -= PLAYER_DAMAGE
    
    def move(self, room, players):
        if not room.id in self.room.adj:
            print("That room is not adjacent.")
            return False
        if room.light:
            print("That room contains light.")
            return False
        
        self.room = room
        for player in players:
            if player.room.id == room.id and not player.damaged:
                player.damage()
        return True

class Gamestate:
    def __init__(self, players, layout):
        self.rooms = []
        self.dracula = Dracula()
        self.players = [Player(i + 1) for i in range(players)]

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
        w = (w + 1) * ROOM_WIDTH
        h = (h + 1) * ROOM_HEIGHT
        grid = [[EMPTY_CHAR] * w for _ in range(h)]

        for room in self.rooms:
            x, y = room.pos 
            x *= ROOM_WIDTH
            y *= ROOM_HEIGHT

            for i in range(ROOM_WIDTH):
                grid[y][x + i] = WALL_CHAR
                grid[y + ROOM_HEIGHT - 1][x + i] = WALL_CHAR
            for i in range(ROOM_HEIGHT - 2):
                grid[y + 1 + i][x] = WALL_CHAR
                grid[y + 1 + i][x + ROOM_WIDTH - 1] = WALL_CHAR
            if room.light:
                grid[y + 2][x + 3] = LIGHT_CHAR
            grid[y][x + 2] = room.name[0]
            grid[y][x + 3] = room.name[1]
        
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

        print("\n".join(["".join(line) for line in grid]) + "\n")
    
    def draw_stats(self, show_dracula=False):
        names = ["Stats"]
        health = ["Health"]
        light = ["Light"]
        stakes = ["Stakes"]
        for i in range(len(self.players)):
            names.append(f"P{self.players[i].id}")
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
        os.system('cls')

        for i in range(len(self.players) - 1, -1, -1):
            if self.players[i].health == 0:
                print(f"Player {self.players[i].id} has died.")
                self.players.pop(i)
            elif self.players[i].damaged:
                print(f"Player {self.players[i].id} was bitten.")
        
        for i in range(len(self.players)):
            if self.players[i].damaged:
                self.players[i].damaged = False
                continue

            self.draw_map(PLAYER_GODMODE)
            self.draw_stats(PLAYER_GODMODE)

            moved = False
            garlic = False

            while True:
                command = input(f"Player {self.players[i].id}: ").split(" ", 1)
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
                    self.draw_map(PLAYER_GODMODE)
                elif action == "stats":
                    self.draw_stats(PLAYER_GODMODE)
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
                    if self.dracula.health == 0:
                        return True
                    break
                elif action == "skip":
                    break
                else:
                    print("Unknown command")
        return False

    def dracula_turn(self):
        os.system('cls')
        if len(self.players) == 0:
            return True
        self.draw_map(True)
        self.draw_stats(True)
        moves = 0
        while True:
            command = input("Dracula: ").split(" ", 1)
            if len(command) == 1:
                command += ['']
            action, arg = command

            if action == "help":
                print("map           View the map.")
                print("stats         View stats of the current players.")
                print("mv     [room] Move into an adjacent room.")
                print("skip          End your turn.")
            elif action == "map":
                self.draw_map(True)
            elif action == "stats":
                self.draw_stats(True)
            elif action == "mv":
                room = self.find_room(arg)
                if room == None:
                    print("That room does not exist.")
                    continue
                moved = self.dracula.move(room, self.players)
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

if __name__ == "__main__":
    main()