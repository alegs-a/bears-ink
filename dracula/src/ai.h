#ifndef AI_H
#define AI_H

#include "room.h"
#include <stdbool.h>


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
    // Rooms that all of the players are in. If two players are in the same
    // room, include that room twice. (that is, counted with mulitplicity)
    struct RoomBuffer all_player_positions;
    // Just those rooms containing players who can be bitten (counted with
    // multiplicity)
    struct RoomBuffer can_bite_player_positions;
    // the rooms that sunlights are cast to and from, counted for multiplicity.
    // The sunlight cast to the room at index i is cast from the room at index
    // i of sunlights_from.
    struct RoomBuffer sunlights_to;
    struct RoomBuffer sunlights_from;
    // true iff Dracula can bite a player on this turn
    bool can_bite;
};


///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                             FUNCTIONS                                   ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////

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
bool dracula_is_present(const Room room);

#endif // AI_H
