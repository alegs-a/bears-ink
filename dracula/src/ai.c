#include "ai.h"
#include "room.h"
#include <stdbool.h>


// number of rounds since the last time Dracula bit a player
static int last_bite = 0; // needs to start at 0 for algos to work
// number of rounds since the last players received POSITIVE information on Dracula's position
static int last_info = 0; // Players know Dracula's starting position
// Distribution of rooms where Dracula could be
static Room dracula_rooms[NUM_ROOMS]; // allocate on the stack
// TODO: set up the rooms buffer and initialize dracula_rooms here
// *dracula_rooms = DUNGEON;
static struct RoomBuffer dracula_state = { .rooms = dracula_rooms, .length = 1 };


/*
 * Find the rooms that Dracula could be in after length steps
 *
 * innaccessible - the rooms that Dracula is not permitted to enter
 * length - the number of moves into an adjacent room that Dracula may make
 * starting - the starting rooms
 *
 * Store the result in starting, assuming that it has enough memory allocated
 * to store EVERY room on the board.
 */
static void walk_ends(
        const struct RoomBuffer innaccessible,
        int length,
        struct RoomBuffer *const starting) {

    for (; length >= 0; length--) {
        // PERF: this is probably pretty damn slow, look here if we are having
        // problems
        for (int i = 0; i < starting->length; i++) {
            for (int j = 0; j < starting->rooms[i].adjacent->length; j++) {
                Room adj = starting->rooms[i].adjacent->rooms[j];
                if (contains_room(innaccessible, adj) < 0)
                    add_no_duplicate(starting, adj);
            }
        }
    }
}


/*
 * Find the least number of moves to get from any room in starting to the end
 * room. If this is impossible or requires more moves than DRACULA_MOVES,
 * return DRACULA_MOVES + 1.
 *
 * Assume that innaccessible and starting are disjoint.
 */
static int shortest_walk(
        const struct RoomBuffer innaccessible,
        const Room end,
        struct RoomBuffer starting) {

    if (contains_room(innaccessible, end) >= 0) return DRACULA_MOVES + 1;

    int num_moves;

    // distribution is the rooms within num_moves of starting. We will mutate
    // this, so make sure we don't mess with starting
    Room room_arr[NUM_ROOMS];
    struct RoomBuffer distribution = room_buffer_copy(starting, room_arr);

    for (num_moves = 0; num_moves <= DRACULA_MOVES; num_moves++) {
        if (contains_room(distribution, end) >= 0) return num_moves;
        walk_ends(innaccessible, 1, &distribution);
    }

    return num_moves;
}


/*
 * This function exists for the case when Dracula starts his turn in a room
 * with sunlight in it.
 *
 * Call at the start of Dracula's turn. If he is in a room with sunlight, move
 * him out of the room, update his state, and return the number of moves he has
 * left on his turn. If there are no ways to move out of the sunlight room,
 * return 0.
 *
 * Otherwise, if Dracula is not in a room with sunlight in it, do nothing and
 * return DRACULA_MOVES.
 */
static int turn_starts(struct GameState st) {
    int sunlight_index = contains_room(st.sunlights_to, dracula_state.rooms[0]);

    // Dracula is not in any room with sunlight
    if (dracula_state.length > 1 || sunlight_index < 0) return DRACULA_MOVES;

    Room buf[NUM_PLAYERS + 1];
    struct RoomBuffer innaccessible = room_buffer_copy(st.sunlights_to, buf);
    add_no_duplicate(&innaccessible, st.sunlights_from.rooms[sunlight_index]);
    walk_ends(innaccessible, 1, &dracula_state);

    // Dracula successfully moved out of the sunlight room
    if (dracula_state.length > 1 || sunlight_index < 0) return DRACULA_MOVES - 1;

    // No way to escape.
    return 0;
}


/*
 * Given that Dracula MUST bite the player in bite_in before any other players
 * (if it is possible), find the best sequence of players to bite.
 *
 * num_moves - the number of moves that Dracula has
 * start - the rooms to start the turn in
 * bite_in - the room to bite a player in to start
 * bites - the buffer containing the bites so far, and to store the next bites
 * ending_distribution - the buffer to store the "safe" rooms to end the turn in
 */
static void best_bite(
        int num_moves,
        const struct RoomBuffer start,
        Room bite_in,
        struct RoomBuffer *const bites,
        struct RoomBuffer *const ending_distribution
        ) {

}
