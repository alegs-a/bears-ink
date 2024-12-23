#ifndef DRACULA_H
#define DRACULA_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "room.h"
#include "ai.h"
#include "rfid.h"

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
    END,
    ACTION_ERROR
};

/**
 * @brief Stores a single turn action.
 */
struct Turn {
    // The action type
    enum Action action;
    // The target room of the action
    enum RoomName room_name;
};

/**
 * @brief Information that is unique to each player.
 */
struct Player {
    // Remaining number of holy water
    uint8_t num_water;
    // Remaining number of sunlights
    uint8_t num_light;
    // True if the player has just been bitten
    bool turn_skipped;
    // True if the player can be bitten
    bool can_bite;
};

// Game rooms and connections
extern Room rooms[ROOM_COUNT];

/**
 * @brief Runs the game.
 */
void dracula_main(void);

/**
 * @brief Check if a given token placed on a specific RFID provides a valid 
 *        action.
 * 
 * @param token The token that will be checked.
 * 
 * @returns True if the action is valid, false otherwise.
 */
bool token_valid(struct Token token);

#endif // DRACULA_H
