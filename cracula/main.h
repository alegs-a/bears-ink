#ifndef DRACULA_H
#define DRACULA_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "room.h"
#include "ai.h"

#define DEBUG

// The stack size of the dracula thread.
#define DRACULA_THREAD_STACK_SIZE 2048

// The thread priority of the dracula logic.
#define DRACULA_THREAD_PRIORITY 5

#define INIT_WATER 2 // The starting number of water for each player
#define MAX_WATER 3 // The maximum number of water

#define INIT_LIGHT 3 // The starting number of light for each player
#define MAX_LIGHT 3 // The maximum number of light

#define PLAYER_HEALTH 5 // The starting health of the players
#define MAX_GARLIC 4 // The total number of garlica for each turn

#define DRACULA_HEALTH 3 // The starting health of dracula
#define DRACULA_MOVES 3 // The number of moves dracula can make per turn

#define PLAYER_COUNT 4 // The number of players in the game
#define ROOM_COUNT 21 // The number of rooms in the game

/**
 * @brief All possible player actions.
 */
enum Action {
    MOVE,
    WATER,
    LIGHT,
    GARLIC,
    END
};

/**
 * @brief Stores a single turn action.
 */
struct Turn {
    enum Action action;
    enum RoomName room_name;
};

/**
 * @brief Information that is unique to each player.
 */
struct Player {
    uint8_t num_water;
    uint8_t num_light;
    bool turn_skipped;
    bool can_bite;
};

extern Room rooms[NUM_ROOMS];

#ifdef DEBUG
extern char *room_names[NUM_ROOMS];
#endif

void dracula_main(void *, void *, void *);

#endif // DRACULA_H
