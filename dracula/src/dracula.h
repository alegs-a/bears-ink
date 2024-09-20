#ifndef DRACULA_H
#define DRACULA_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// The stack size of the dracula thread.
#define DRACULA_THREAD_STACK_SIZE 2048

// The thread priority of the dracula logic.
#define DRACULA_THREAD_PRIORITY 5

#define INIT_WATER 2
#define INIT_LIGHT 3
#define MAX_WATER 3
#define MAX_LIGHT 3
#define MAX_GARLIC 4
#define PLAYER_HEALTH 5

#define DRACULA_HEALTH 3
#define DRACULA_MOVES 3

#define PLAYER_COUNT 4
#define ROOM_COUNT 21

// All possible actions the player can do
enum player_actions {
    MOVE,
    WATER,
    LIGHT,
    GARLIC,
    END
};

// All room names
enum room_name {
    NHALL,
    TOMB,
    GUARDEDWAY,
    GALLERY,
    ALLEY,
    BONEPIT,
    ENTRANCE,
    VENT,
    DUNGEON,
    DINING,
    LIBRARY,
    CRYPT,
    PASSAGE,
    CHAPEL,
    NEST,
    BATHROOM,
    CANAL,
    STAIRCASE,
    CELLAR,
    SHALL,
    BALLROOM
};

// Information about a single turn action
struct turn {
    enum player_actions action;
    enum room_name room_name;
};

// Defines connections between rooms
struct room {
    enum room_name name;
    struct room_buffer *adjacent;
};

// A buffer of room
struct room_buffer {
    struct room **rooms;
    size_t length;
};

// Information unique to each player
struct player {
    uint8_t num_water;
    uint8_t num_light;
    struct room *room;
    bool turn_skipped;
};

// Information about the current game state
struct gamestate {
    struct player *players;
    uint8_t player_health;
    uint8_t garlic;
    uint8_t dracula_health;
    bool can_bite;
};

bool dracula_init();

void dracula_main(void *, void *, void *);

#endif // DRACULA_H
