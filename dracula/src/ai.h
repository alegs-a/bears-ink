#ifndef AI_H
#define AI_H

#include "room.h"
#include <stdbool.h>



///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                             CONSTANTS                                   ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////


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


#ifndef GAME_STATE
#define GAME_STATE

struct GameState {
    // rooms that all of the players who can be bitten are in. If two players
    // are in the same room, include that room twice. (that is, counted for
    // mulitplicity)
    struct RoomBuffer player_positions;

    // the rooms that sunlights are cast to and from, counted for multiplicity.
    // The sunlight cast to the room at index i is cast from the room at index
    // i of sunlights_from.
    struct RoomBuffer sunlights_to;
    struct RoomBuffer sunlights_from;

    // true iff Dracula can bite a player on this turn
    bool can_bite;

    // number of rounds since the last time Dracula bit a player
    int last_bite;

    // number of rounds since the last players received POSITIVE information on
    // Dracula's position
    int last_info;
};
#endif



///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                             FUNCTIONS                                   ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////


/*
 * Dracula completes his turn and updates his internal state.
 *
 * st - the current game state
 *
 * Return the buffer of rooms that Dracula bites players in. The list of rooms
 * does not contain any duplicates.
 */
struct RoomBuffer dracula_turn(struct GameState st);


/*
 * Dracula returns if he is present and updates his internal state.
 *
 * st - the current game state.
 *
 * room - the room to check if Dracula is in
 */
bool dracula_is_present(struct GameState st, Room room);

#endif // AI_H
