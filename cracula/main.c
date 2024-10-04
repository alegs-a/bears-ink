#include "main.h"

// static K_THREAD_DEFINE(dracula, DRACULA_THREAD_STACK_SIZE,
//     dracula_main, NULL, NULL, NULL, DRACULA_THREAD_PRIORITY, 0, 0);

// Defined and initialised be the above macro.
//extern const k_tid_t dracula_thread_id;

// Intialise rooms and connections
Room rooms[ROOM_COUNT] = {
    {.room=NHALL, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[TOMB], &rooms[ENTRANCE]}[0]}},
    {.room=TOMB, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[NHALL], &rooms[GUARDEDWAY], &rooms[BONEPIT]}[0]}},
    {.room=GUARDEDWAY, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[GALLERY], &rooms[TOMB]}[0]}},
    {.room=GALLERY, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[GUARDEDWAY], &rooms[ALLEY], &rooms[BALLROOM]}[0]}},
    {.room=ALLEY, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[BONEPIT], &rooms[GALLERY], &rooms[DUNGEON]}[0]}},
    {.room=BONEPIT, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[TOMB], &rooms[ALLEY]}[0]}},
    {.room=ENTRANCE, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[NHALL], &rooms[VENT], &rooms[LIBRARY]}[0]}},
    {.room=VENT, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[ENTRANCE], &rooms[DUNGEON], &rooms[CRYPT]}[0]}},
    {.room=DUNGEON, .adjacent=&(struct RoomBuffer){.length=4, .rooms=&(Room*[4]){&rooms[VENT], &rooms[ALLEY], &rooms[DINING], &rooms[BATHROOM]}[0]}},
    {.room=DINING, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[DUNGEON], &rooms[BALLROOM]}[0]}},
    {.room=LIBRARY, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[ENTRANCE], &rooms[PASSAGE]}[0]}},
    {.room=CRYPT, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[VENT], &rooms[CHAPEL]}[0]}},
    {.room=PASSAGE, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[LIBRARY], &rooms[CHAPEL], &rooms[SHALL]}[0]}},
    {.room=CHAPEL, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[CRYPT], &rooms[PASSAGE], &rooms[NEST]}[0]}},
    {.room=NEST, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[CHAPEL], &rooms[BATHROOM]}[0]}},
    {.room=BATHROOM, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[DUNGEON], &rooms[NEST], &rooms[CANAL]}[0]}},
    {.room=CANAL, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[BATHROOM], &rooms[CELLAR]}[0]}},
    {.room=STAIRCASE, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[CELLAR], &rooms[BALLROOM]}[0]}},
    {.room=CELLAR, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[STAIRCASE], &rooms[CANAL], &rooms[SHALL]}[0]}},
    {.room=SHALL, .adjacent=&(struct RoomBuffer){.length=2, .rooms=&(Room*[2]){&rooms[PASSAGE], &rooms[CELLAR]}[0]}},
    {.room=BALLROOM, .adjacent=&(struct RoomBuffer){.length=3, .rooms=&(Room*[3]){&rooms[DINING], &rooms[GALLERY], &rooms[STAIRCASE]}[0]}}
};

static void full_dracula_turn(struct GameState *gamestate);
static void full_players_turn(struct GameState *gamestate);

/**
 * @brief Runs the game.
 */
int main() {
    dracula_setup();

    // Initialise players
    struct Player players[PLAYER_COUNT] = {
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .turn_skipped=false, .can_bite=true},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .turn_skipped=false, .can_bite=true},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .turn_skipped=false, .can_bite=true},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .turn_skipped=false, .can_bite=true}
    };

    //Initialise player positions
    struct RoomBuffer player_pos = {.length=NUM_PLAYERS, .rooms=&(Room*[PLAYER_COUNT])
        {&rooms[NHALL], &rooms[GUARDEDWAY], &rooms[CELLAR], &rooms[PASSAGE]}[0]};

    // Initialise gamestate
    struct GameState gamestate = {.player_health=PLAYER_HEALTH, 
        .garlic=MAX_GARLIC, .dracula_health=DRACULA_HEALTH, .can_bite=true, 
        .players=&players[0], .player_positions=player_pos,
        .can_bite_player_positions=player_pos,
        .sunlights_from={.length=0, .rooms=&(Room*[PLAYER_COUNT]){NULL, NULL, NULL, NULL}[0]},
        .sunlights_to={.length=0, .rooms=&(Room*[PLAYER_COUNT]){NULL, NULL, NULL, NULL}[0]}};

    // Main Game Loop
    for (;;) {
        full_players_turn(&gamestate);
        full_dracula_turn(&gamestate);

        //Handle game ending
        if (gamestate.player_health <= 0) {
            //TODO handle game loss
            printf("Dracula wins!");
            break;
        } else if (gamestate.dracula_health <= 0) {
            //TODO handle game win
            printf("Player's win!");
            break;
        }
    }
    return 0;
}

/**
 * @brief Check if a room is adjacent to another room.
 *
 * @param src The source room that will have its adjacency array looped over.
 * @param dst The destination room the other source room will be compared 
 * against for adjacency.
 * 
 * @returns True if the rooms are adjacent, false otherwise.
 */
static bool is_adjacent(enum RoomName src, enum RoomName dst) {
    for (uint8_t i = 0; i < rooms[src].adjacent->length; i++) {
        if (rooms[src].adjacent->rooms[i]->room == dst) {
            return true;
        }    
    }
    return false;
}

/**
 * @brief Retrieves the commited action from the player.
 *
 * @returns A the current turn action which conatins the action to do and 
 * which room it will take place in.
 */
static struct Turn player_input() {
    // TODO make input from RFID readers instead of scanf
    char action[3];
    printf("Input Action:\n");
    scanf("%s", &action);
    uint8_t action_val = atoi(action);

    char room[2];
    printf("Input Room:\n");
    scanf("%s", &room);
    uint8_t room_val = atoi(room);

    return (struct Turn){.action=action_val, .room_name=room_val};
} 

/**
 * @brief Handles the rest action of the current player. 
 * 
 * This action involves the player gaining a resource of their choice.
 *
 * @param player The current player number.
 * @param gamestate The current board gamestate.
 */
static void player_rest(uint8_t player, struct GameState *gamestate) {
    for(;;) {
        // TODO make input from RFID readers instead of scanf
        char resource[3];
        printf("Choose Resource:\n");
        scanf("%s", &resource);
        int resource_val = atoi(resource);

        if (resource_val == WATER) {
            if (gamestate->players[player].num_water < MAX_WATER) {
                gamestate->players[player].num_water++;
            }
            break;
        } else if (resource_val == LIGHT) {
            if (gamestate->players[player].num_light < MAX_LIGHT) {
                gamestate->players[player].num_light++;
            }
            break;
        } else {
            printf("Invalid Resource\n");
        }
    }
}

/**
 * @brief Handles the throwing holy water action from the current player.
 * 
 * This action is the only way to damage dracula. Doing this action ends the
 * players turn immediately. This also reveals whether dracula was in that 
 * room.
 * 
 * @param player The current player number.
 * @param gamestate The current board gamestate.
 * @param room The room to throw holy water in.
 * 
 * @returns True if throwing holy water was successful otherwise false.
 */
static bool throw_water(uint8_t player, struct GameState *gamestate, enum RoomName room) {
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room) &&
            gamestate->player_positions.rooms[player]->room != room) {
        printf("The room is not adjacent\n");
        return false;
    }
    if (gamestate->players[player].num_water <= 0) {
        printf("No more water\n");
        return false;
    }

    if (dracula_is_present(&rooms[room])) {
        gamestate->dracula_health--;
        gamestate->can_bite = false;
        printf("Dracula is in that room with now %d health.\n", gamestate->dracula_health);
    } else {
        printf("Dracula is not in that room.\n");
    }
    gamestate->players[player].num_water--;
    return true;
}

/**
 * @brief Handles the creating light action from the current player.
 * 
 * This action creates light to stop dracula from entering that room during 
 * his turn.
 * 
 * @param player The current player number.
 * @param gamestate The current board gamestate.
 * @param room The room to create light in.
 * 
 * @returns True if creating light was successful otherwise false.
 */
static bool create_light(uint8_t player, struct GameState *gamestate, enum RoomName room) {
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room)) {
        printf("The room is not adjacent\n");
        return false;
    }
    if (gamestate->players[player].num_light <= 0) {
        printf("No more light\n");
        return false;
    }

    if (dracula_is_present(&rooms[room])) {
        printf("Dracula is in that room with %d health.\n", gamestate->dracula_health);
    } else {
        printf("Dracula is not in that room.\n");
    }
    add_with_duplicate(&(gamestate->sunlights_from), gamestate->player_positions.rooms[player]);
    add_with_duplicate(&(gamestate->sunlights_to), &rooms[room]);
    gamestate->players[player].num_light--;
    return true;
}

/**
 * @brief Handles the throwing garlic action from the current player.
 * 
 * This action allows players to check whether dracula is a chosen room. The
 * number of times this can be done is shared between all players. The board
 * will output the result of the action.
 * 
 * @param player The current player number.
 * @param gamestate The current board gamestate.
 * @param room The room to throw garlic into.
 * 
 * @returns True if throwing garlic was succesful otherwise false.
 */
static bool throw_garlic(uint8_t player, struct GameState *gamestate, enum RoomName room) {
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room) &&
            gamestate->player_positions.rooms[player]->room != room) {
        printf("The room is not adjacent\n");
        return false;
    }
    if (gamestate->garlic <= 0) {
        printf("No more garlic\n");
        return false;
    }

    if (dracula_is_present(&rooms[room])) {
        printf("Dracula is in that room with %s health.\n", gamestate->dracula_health);
    } else {
        printf("Dracula is not in that room.\n");
    }
    gamestate->garlic--;
    return true;
}

/**
 * @brief Handles the move action of the current player. 
 * 
 * This action allows the player to move to an adjacent room. This can only be
 * done once per turn.
 *
 * @param player The current player number.
 * @param gamestate The current board gamestate.
 * @param room The room to move into.
 * 
 * @returns True if the movement was succesful otherwise false.
 */
static bool player_move(uint8_t player, struct GameState *gamestate, enum RoomName room) {
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room)) {
        printf("The room is not adjacent\n");
        return false;
    }
    gamestate->player_positions.rooms[player] = &rooms[room];
    return true;
}

/**
 * @brief Handles the turn of the current player.
 * 
 * The board will keep requesting for an action until a valid turn ending 
 * action is done (END, WATER or LIGHT). A player can rest if they end their
 * turn without having done any other actions.
 *
 * @param player The current player number.
 * @param gamestate The current board gamestate.
 */
static void player_turn(uint8_t player, struct GameState *gamestate) {
     if (!gamestate->players[player].can_bite) {
        gamestate->players[player].can_bite = true;
    }
    if (gamestate->players[player].turn_skipped) {
        gamestate->players[player].turn_skipped = false;
        gamestate->players[player].can_bite = false;
        return;
    }
    printf("Player %d's turn.\n", player);

    bool player_moved = false;
    bool garlic_thrown = false;
    for (;;) {
        struct Turn turn = player_input();
        if (turn.action == END) {
            // Rest occurs when no action has been done
            if (!player_moved && !garlic_thrown) { 
                player_rest(player, gamestate);
            }
            break;
        }
        else if (turn.action == WATER && throw_water(player, gamestate, turn.room_name)) {
            break;
        }
        else if (turn.action == LIGHT && create_light(player, gamestate, turn.room_name)) {
            break;
        }
        else if (turn.action == GARLIC && throw_garlic(player, gamestate, turn.room_name)) {
            garlic_thrown = true;
        }
        else if (turn.action == MOVE && player_move(player, gamestate, turn.room_name)) {
            player_moved = true;
        }
    }
}

/**
 * @brief Handles all the players turns for the current round.
 * 
 * Updates gamestate garlic numbers and player positions first, 
 * then iteratively runs each player's turn.
 *
 * @param gamestate The current board gamestate.
 */
static void full_players_turn(struct GameState *gamestate) {
    gamestate->garlic = MAX_GARLIC;
    gamestate->can_bite_player_positions.length = 0;
    for (uint8_t i = 0; i < NUM_PLAYERS; i++) {
        player_turn(i, gamestate);
        if (gamestate->players[i].can_bite) {
            add_with_duplicate(&(gamestate->can_bite_player_positions), gamestate->player_positions.rooms[i]);
        }
        // Checks if the game is over
        if (gamestate->dracula_health <= 0) {
            break;
        } 
    }
}

/**
 * @brief Handles Dracula's turn.
 * 
 * Retrieves information from the Dracula AI model where each bite occured.
 * Each bite then gets applied to each player if they were in that room.
 *
 * @param gamestate The current board gamestate.
 */
static void full_dracula_turn(struct GameState *gamestate) {
    struct RoomBuffer bites = {.length=0, .rooms=&(Room*[PLAYER_COUNT]){NULL, NULL, NULL, NULL}[0]};
    dracula_turn(gamestate, &bites);
    
    // Applies the bites to each player when applicable
    if (bites.length != 0) {
        gamestate->player_health--;
        for (uint8_t i = 0; i < NUM_PLAYERS; i++) {
            for (uint8_t j = 0; j < bites.length; j++) {
                if (gamestate->player_positions.rooms[i]->room == bites.rooms[j]->room) {
                    gamestate->players[i].turn_skipped = true;

                    // Players lose one water per bite
                    if (gamestate->players[i].num_water > 0) {
                        gamestate->players[i].num_water--;   
                    }
                    printf("Player %d has been bitten and loses one water. Player's now have %d health.\n", i,  gamestate->player_health);
                    break;
                }               
            }
        }
    }    
    gamestate->can_bite = true; 
    gamestate->sunlights_from.length = 0;
    gamestate->sunlights_to.length = 0;
}