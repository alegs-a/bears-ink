#include "ai.h"
#include "room.h"
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>

// #define DEBUG

#ifndef DEBUG
    #include "dracula.h"
    #include <zephyr/sys/printk.h>
    #define ASSERT(b, err_str) if (!(b)) printk("Assertion error: %s at (%s:%d)\n", err_str, __FILE__, __LINE__);
#else
    #include "../../prototypes/debug/debug.h"
    #include <stdio.h>
    #define ASSERT(b, err_str) if (!(b)) fprintf(stderr, "Assertion error: %s at (%s:%d)\n", err_str, __FILE__, __LINE__);
#endif


// number of rounds since the last time Dracula bit a player
static int last_bite;
// number of rounds since the last players received POSITIVE information on Dracula's position
static int last_info;
// Distribution of rooms where Dracula could be
static Room **dracula_rooms;
static struct RoomBuffer dracula_state;


#ifdef DEBUG
static void print_room_buffer(const struct RoomBuffer buf) {
    for (int i = 0; i < buf.length; i++) {
        printf("%s\n", room_names[buf.rooms[i]->room]);
    }
    printf("\n");
}
#endif


void dracula_setup(void) {
    last_bite = 0; // needs to start at 0 for algos to work
    last_info = 0; // Players know Dracula's starting position
    dracula_rooms = malloc(NUM_ROOMS * sizeof(Room*));
    dracula_rooms[0] = &rooms[DUNGEON];
    dracula_state.length = 1;
    dracula_state.rooms = dracula_rooms;
    #ifdef DEBUG
    srand(time(NULL));
    #endif
}


void dracula_cleanup(void) {
    free(dracula_rooms);
}

/*
 * @brief Find the the rooms that can be reached in AT MOST length number of
 *  moves (without passing through any room in innaccessible) and store the
 *  result in start.
 *
 * @note starting and innaccessible must be disjoint
 *
 * @param[in] innaccessible : the rooms that Dracula is not permitted to enter
 * @param[in] length : the maximum number of moves into an adjacent room
 * @param[in,out] starting - the starting rooms. Store the result in here
 */
static void walk_ends(
        const struct RoomBuffer innaccessible,
        int length,
        struct RoomBuffer *const starting) {

    while (length > 0) {
        int len = starting->length;
        for (int i = 0; i < len; i++) {
            for (int j = 0; j < starting->rooms[i]->adjacent->length; j++) {
                Room *candidate = starting->rooms[i]->adjacent->rooms[j];
                if (contains_room(innaccessible, candidate) < 0) {
                    add_no_duplicate(starting, candidate);
                }
            }
        }
        length--;
    }
}


/*
 * @brief Find the least number of moves to get from any room in starting to the
 *  end room, without entering any innaccessible rooms. If this is impossible or
 *  requires more moves than DRACULA_MOVES,
 *  return DRACULA_MOVES + 1.
 * 
 * @note starting and innaccessible must be disjoint
 *
 * @param[in] innaccessible : the rooms that Dracula may not enter
 * @param[in] end : the room to find the least number of moves required to reach
 * @param[in] starting : collection of rooms in which Dracula may start
 */
static int shortest_walk(
        const struct RoomBuffer innaccessible,
        const Room *end,
        const struct RoomBuffer starting) {

    if (contains_room(innaccessible, end) >= 0) return DRACULA_MOVES + 1;

    // distribution is the rooms within num_moves of starting. We will mutate
    // this, so make sure we don't mess with starting
    Room **room_arr = malloc(NUM_ROOMS * sizeof(Room*));
    struct RoomBuffer distribution = room_buffer_from(starting, room_arr);

    for (int num_moves = 0; num_moves <= DRACULA_MOVES; num_moves++) {
        if (contains_room(distribution, end) >= 0) {
            free(room_arr);
            return num_moves;
        }
        walk_ends(innaccessible, 1, &distribution);
    }

    free(room_arr);
    return DRACULA_MOVES + 1;
}


/*
 * @brief this function exists for the case when Dracula starts his turn in a
 *  room with sunlight in it. Call at the start of Dracula's turn. If he is in a
 *  room with sunlight, move him out of the room and update his state.
 *  Otherwise, if Dracula is not in a room with sunlight in it, do nothing.
 *
 * @note this function reads and modifies internal dracula state.
 *
 * @param[in] st : the current game state
 *
 * @return the number of moves he has left on his turn. If there are no ways to
 *  move out of the sunlight room, return 0.
 */
static int turn_starts(const struct GameState *st) {
    int sunlight_index = contains_room(st->sunlights_to, dracula_state.rooms[0]);

    // Dracula is not in any room with sunlight
    if (dracula_state.length > 1 || sunlight_index < 0) return DRACULA_MOVES;

    Room **innacc_buf = malloc(NUM_PLAYERS * sizeof(Room*));
    struct RoomBuffer innaccessible = room_buffer_from(st->sunlights_to, innacc_buf);
    add_no_duplicate(&innaccessible, st->sunlights_from.rooms[sunlight_index]);
    walk_ends(innaccessible, 1, &dracula_state);

    // Dracula successfully moved out of the sunlight room
    if (dracula_state.length > 1 || sunlight_index < 0) {
        free(innacc_buf);
        return DRACULA_MOVES - 1;
    }

    // No way to escape.
    free(innacc_buf);
    return 0;
}


/*
 * @brief Given that Dracula MUST bite the player in bite_in before any other
 *  players (if it is possible), find the best sequence of players to bite.
 *
 * @param[in] num_moves : the number of moves that Dracula has remaining
 * @param[in] st : a pointer to the current game state
 * @param[in] start : the rooms to start the turn in
 * @param[in] bite_in : the room to bite a player in to start. This is not
 *  already loaded into bites.
 * @param[in] player_positions : the positions of all player who have not yet
 *  been bitten this turn, but can be. This includes bite_in
 * @param[out] bites : the buffer containing the bites so far, and to store the
 *  next bites
 * @param[out] ending_distribution : the buffer to store what Dracula's state
 *  should be if this bite is made.
 */
static void best_bite(
        int num_moves,
        const struct GameState *st,
        struct RoomBuffer start,
        Room *bite_in,
        struct RoomBuffer player_positions,
        struct RoomBuffer *bites,
        struct RoomBuffer *ending_distribution
        ) {

    // Compute innaccessible rooms for this step
    Room **innacc_buf = malloc(2*NUM_PLAYERS * sizeof(Room*));
    struct RoomBuffer innacc = room_buffer_from(st->sunlights_to, innacc_buf);
    remove_if_present(&player_positions, bite_in);
    concat_no_duplicate(&innacc, player_positions);

    // moves after this bite
    int remaining_moves = num_moves - shortest_walk(innacc, bite_in, start);

    // Base case:
    if (remaining_moves <= 0) {
        free(innacc_buf);
        return;
    }

    // Recursive step: either do nothing, or bite another player
    add_with_duplicate(bites, bite_in);

    // Do nothing
    ending_distribution->rooms[0] = bite_in;
    ending_distribution->length = 1;
    walk_ends(innacc, remaining_moves, ending_distribution);
    free(innacc_buf);
    // remove the rooms which are adjacent to some player
    Room **adj_buf = malloc(MAX_ADJ * sizeof(Room*));
    struct RoomBuffer unsafe; unsafe.rooms = adj_buf;
    // TODO: this makes the state correct, but ignores players who cannot be bitten
    for (int i = 0; i < player_positions.length; i++) {
        unsafe.rooms[0] = player_positions.rooms[i];
        unsafe.length = 1;
        walk_ends(EMPTY_BUFFER, 1, &unsafe);
        for (int j = 0; j < unsafe.length; j++) {
            remove_if_present(ending_distribution, unsafe.rooms[j]);
        }
    }
    free(adj_buf);

    // Bite another player on this turn
    Room **positions_buf = malloc(NUM_PLAYERS * sizeof(Room*));
    Room **bites_buf = malloc(NUM_PLAYERS * sizeof(Room*));
    Room **ending_buf = malloc(NUM_ROOMS * sizeof(Room*));
    for (int i = 0; i < player_positions.length; i++) {
        // setup start
        start.rooms[0] = bite_in;
        start.length = 1;
        struct RoomBuffer ending_copy = { .rooms = ending_buf, .length = 0 };
        struct RoomBuffer positions_copy = room_buffer_from(player_positions, positions_buf);
        struct RoomBuffer bites_copy = room_buffer_from(*bites, bites_buf);
        best_bite(
                remaining_moves,
                st,
                start,
                player_positions.rooms[i],
                positions_copy,
                &bites_copy,
                &ending_copy);

        // The best option is stored in bites and ending distribution
        if (bites_copy.length > bites->length
                || (bites_copy.length == bites->length
                    && ending_copy.length > ending_distribution->length)) {
            room_buffer_copy(bites, bites_copy);
            room_buffer_copy(ending_distribution, ending_copy);
        }
    }
    free(positions_buf);
    free(bites_buf);
    free(ending_buf);
}


/**
 * @brief Return a random floating point number between 0 and 1 (that is, sample
 * the standard uniform distribution)
 */
static inline float std_unif(void) {
    // TODO: get this bad boy to use hardware randomness
    return (float)rand() / (float)RAND_MAX;
}


/**
 * @brief Supposing Dracula must bite a player on his turn, determine the bite
 *  he will make on his turn using random weights and heuristics for number of
 *  players bitten, number of safe rooms in which to end the turn. If Dracula
 *  can bite more than one player on his turn, make that move.
 *
 * @param[in] num_moves : the number of moves Dracula has remaining in his turn
 * @param[in] st : a pointer to the current game state
 * @param[out] score : the score for the bite. This will be 0 if Dracula can
 *  make more than one bite, otherwise rand() * 1/(number of safe rooms)
 *  (say 1/0 = 1)
 * @param[out] bites : the rooms in which Dracula bites players
 * @param[out] ending_distribution : the ending distribution for Dracula
 *
 * @return if it is possible for Dracula to make ANY bite on his turn.
 */
static bool bite(
        int num_moves,
        const struct GameState *st,
        float *score,
        struct RoomBuffer *bites,
        struct RoomBuffer *ending_distribution) {

    if (st->can_bite_player_positions.length == 0) return false;

    // Base case for comparison
    bites->length = 0;
    ending_distribution->length = 0;
    *score = 1;

    // In-place sort of best bite option
    Room **start_buf = malloc(NUM_ROOMS * sizeof(Room*));
    Room **positions_buf = malloc(NUM_PLAYERS * sizeof(Room*));
    Room **bites_buf = malloc(NUM_PLAYERS * sizeof(Room*));
    Room **ending_buf = malloc(NUM_ROOMS * sizeof(Room*));
    struct RoomBuffer start_copy;
    struct RoomBuffer positions_copy;
    struct RoomBuffer bites_copy;
    struct RoomBuffer ending_copy;
    bites_copy.rooms = bites_buf;
    ending_copy.rooms = ending_buf;
    for (int i = 0; i < st->can_bite_player_positions.length; i++) {
        start_copy = room_buffer_from(dracula_state, start_buf);
        positions_copy = room_buffer_from(st->can_bite_player_positions, positions_buf);
        bites_copy.length = 0;
        ending_copy.length = 0;
        best_bite(
                num_moves,
                st,
                start_copy,
                st->can_bite_player_positions.rooms[i],
                positions_copy,
                &bites_copy,
                &ending_copy);

        // no bites.
        if (bites_copy.length == 0) continue;

        // score for safe rooms to end the turn
        // Don't round the score to 0 here - we want to compare the best way to
        // bite more than one player
        float safe = ending_copy.length ? ending_copy.length : 1;
        safe = std_unif() / safe;

        // in-place lex comparison - smaller score is better
        if (bites_copy.length > bites->length
                || (bites_copy.length == bites-> length && safe < *score)) {
            room_buffer_copy(bites, bites_copy);
            room_buffer_copy(ending_distribution, ending_copy);
            *score = safe;
        }
    }

    free(start_buf);
    free(positions_buf);
    free(bites_buf);
    free(ending_buf);

    // Now we can set the score to 0 (if necessary)
    if (bites->length > 1) *score = 0;
    return bites->length != 0; // false iff there is no possible bite
}


void dracula_turn(const struct GameState *st, struct RoomBuffer *bites) {
    last_bite++;
    last_info++;
    float bite_roll = ((float)last_bite / (float)WITHOUT_BITE) * std_unif();
    int num_moves = turn_starts(st);
    bites->length = 0;

    ASSERT(dracula_state.length > 0, "invalid Dracula state");
    // Literally nothing that Dracula can do
    if (num_moves <= 0) return;

    // Run the heuristic
    float bite_score;
    Room **ending = malloc(sizeof(Room*) * NUM_ROOMS);
    struct RoomBuffer ending_distribution;
    ending_distribution.rooms = ending;
    bool can_bite = bite(num_moves, st, &bite_score, bites, &ending_distribution);

    if (!can_bite || bite_score > bite_roll || !(st->can_bite)) { // no bite
        // update Dracula's state
        Room **innacc_buf = malloc(2*NUM_PLAYERS * sizeof(Room*));
        struct RoomBuffer innacc = room_buffer_from(st->sunlights_to, innacc_buf);
        // If Dracula is not allowed to bite, he can move through player rooms
        if (st->can_bite) concat_no_duplicate(&innacc, st->can_bite_player_positions);
        walk_ends(innacc, num_moves, &dracula_state);
        free(innacc_buf);
        bites->length = 0;
    } else { // BITE!
        remove_duplicate_rooms(bites);
        room_buffer_copy(&dracula_state, ending_distribution);
        // Edge case for if we do the bite, but there are NO safe rooms
        // NOTE: At the moment, this is dead code. However, in an implementation
        // where we don't take any shortcuts, it is necessary.
        if (dracula_state.length == 0) {
            add_with_duplicate(&dracula_state, bites->rooms[bites->length - 1]);
        }
        last_bite = 0;
        last_info = 0;
    }
    free(ending);

    #ifdef DEBUG
    printf("Dracula state:\n");
    print_room_buffer(dracula_state);
    #endif
}


bool dracula_is_present(Room *room) {
    ASSERT(dracula_state.length > 0, "invalid Dracula state");

    // Dracula cannot possibly be in the room
    if (contains_room(dracula_state, room) < 0) return false;

    float info_threshold = (float)last_info / (float)(dracula_state.length * WITHOUT_INFO);
    float bite_threshold = (float)last_bite / (float)(WITHOUT_BITE);
    float info_roll = std_unif();
    float bite_roll = std_unif();

    // Information given! update the state and last_info
    if (dracula_state.length == 1 // Dracula necessarily present
            || (info_roll <= info_threshold && bite_roll <= bite_threshold)) {
        dracula_state.rooms[0] = room;
        dracula_state.length = 1;
        last_info = 0;
        return true;
    }

    remove_if_present(&dracula_state, room);

    #ifdef DEBUG
    printf("Dracula state:\n");
    print_room_buffer(dracula_state);
    #endif

    return false;
}
