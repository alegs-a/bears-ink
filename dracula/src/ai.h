#ifndef AI_H
#define AI_H

#include "room.h"
#include <stdbool.h>
#include <stdint.h>


///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                             CONSTANTS                                   ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////

#define NUM_PLAYERS 4 // the number of players in the game.
#define PASSIVENESS 2 // Generally just controls likelihoods for detection and
                      // bites

#define DRACULA_MOVES 3 // The number of spaces Dracula can move on his turn

// Constants controlling on how much more aggressive Dracula gets depending on:
#define WITHOUT_BITE 5 // the last bite
#define WITHOUT_INFO 4 // the last POSITIVE information (bites, detection with
                       // sunlight or a garlic


///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                               TYPES                                     ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////

struct GameState {
    // A buffer of players
    struct Player *players;
    // The remaining player health
    int8_t player_health;
    // The remaning number of garlics
    uint8_t garlic;
    // True if the current player is resting
    bool player_resting;
    // The current player index
    uint8_t cur_player;
    // The remaining Dracula health
    int8_t dracula_health;
    // Rooms that all of the players are in. If two players are in the same
    // room, include that room twice. (that is, counted with mulitplicity)
    struct RoomBuffer player_positions;
    // Just those rooms containing players who can be bitten (counted with
    // multiplicity)
    struct RoomBuffer can_bite_player_positions;
    // the rooms that sunlights are cast to and from, counted for multiplicity.
    // The sunlight cast to the room at index i is cast from the room at index
    // i of sunlights_from.
    struct RoomBuffer sunlights_to;
    struct RoomBuffer sunlights_from;
    struct RoomBuffer garlic_rooms;
    // true iff Dracula can bite a player on this turn
    bool can_bite;
};


///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                             FUNCTIONS                                   ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////

/**
 * @brief Call this at the start of a game to setup the intial internal Dracula
 * AI state.
 */
void dracula_setup(void);

/**
 * @brief Call this a the end of a game for Dracula to free any resources used
 * before the next run.
 */
void dracula_cleanup(void);

/**
 * @brief Dracula completes his turn and updates his internal state. Write to
 * bites the rooms that Dracula bites players in.
 *
 * @note The list of rooms does not contain any duplicates.
 *
 * @param[in] st the current game state
 * @param[out] bites a buffer with enough memory for 4 rooms
 */
void dracula_turn(const struct GameState *st, struct RoomBuffer *bites);

/**
 * @brief Dracula returns if he is present and updates his internal state.
 *
 * @param room[in] the room to check if Dracula is present
 *
 * @return true if and only if Dracula is present in the given room.
 */
bool dracula_is_present(Room *room);

#endif // AI_H
