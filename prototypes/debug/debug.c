#include "../../dracula/src/ai.h"
#include "../../dracula/src/room.h"
#include "debug.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>


// Intialise rooms and connections
Room rooms[NUM_ROOMS] = {
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

char *room_names[] = {
    "NHALL",
    "TOMB",
    "GUARDEDWAY",
    "GALLERY",
    "ALLEY",
    "BONEPIT",
    "ENTRANCE",
    "VENT",
    "DUNGEON",
    "DINING",
    "LIBRARY",
    "CRYPT",
    "PASSAGE",
    "CHAPEL",
    "NEST",
    "BATHROOM",
    "CANAL",
    "STAIRCASE",
    "CELLAR",
    "SHALL",
    "BALLROOM",
    "RESOURCE"
};

char *token_type_names[] = {
    "Player1",
    "Player2",
    "Player3",
    "Player4",
    "Garlic",
    "Sunlight",
    "HolyWater"
};

// Game state with its corresponding mutex lock
struct GameState gamestate;

static void full_dracula_turn(struct GameState *gamestate);
static void full_players_turn(struct GameState *gamestate);

/**
 * @brief Runs the game.
 */
int main(void) {
    // Initialise the AI
    dracula_setup();
  
    // Initialise gamestate
    gamestate = (struct GameState){.player_health=PLAYER_HEALTH, 
        .garlic=MAX_GARLIC, .dracula_health=DRACULA_HEALTH, 
        .can_bite=true, .cur_player=0, .player_resting = false,
        .sunlights_from={.length=0, .rooms=&(Room*[PLAYER_COUNT]){NULL, NULL, NULL, NULL}[0]},
        .sunlights_to={.length=0, .rooms=&(Room*[PLAYER_COUNT]){NULL, NULL, NULL, NULL}[0]}};

    // Initialise players
    gamestate.players = &(struct Player [PLAYER_COUNT]){
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .turn_skipped=false, .can_bite=true},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .turn_skipped=false, .can_bite=true},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .turn_skipped=false, .can_bite=true},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .turn_skipped=false, .can_bite=true}}[0];
    
    //Initialise player positions
    gamestate.player_positions = (struct RoomBuffer){.length=NUM_PLAYERS, .rooms=&(Room*[PLAYER_COUNT])
        {&rooms[NHALL], &rooms[GUARDEDWAY], &rooms[CELLAR], &rooms[PASSAGE]}[0]};
    gamestate.can_bite_player_positions = (struct RoomBuffer){.length=NUM_PLAYERS, .rooms=&(Room*[PLAYER_COUNT])
        {&rooms[NHALL], &rooms[GUARDEDWAY], &rooms[CELLAR], &rooms[PASSAGE]}[0]};

    // Main Game Loop
    for (;;) {
        full_players_turn(&gamestate);
        full_dracula_turn(&gamestate);

        //Handle game ending
        if (gamestate.player_health <= 0) {
            printf("Dracula wins!\n");
            break;
        } else if (gamestate.dracula_health <= 0) {
            printf("Player's win!\n");
            break;
        }
    }

    dracula_cleanup();
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
 * @brief Check if a given token provides a valid action.
 * 
 * @param token The token that will be checked.
 * 
 * @returns True if the action is valid, false otherwise.
 */
bool token_valid(struct Token token) {
    uint8_t player = gamestate.cur_player;
    enum RoomName room = token.room;
    bool valid = true;

    if (!gamestate.player_resting) {
        if (token.room == NUM_ROOMS) {
            valid = false;
        } else if (token.kind == Garlic) {
            if (!is_adjacent(gamestate.player_positions.rooms[player]->room, room) &&
                gamestate.player_positions.rooms[player]->room != room) {
                valid = false;
            } else if (gamestate.garlic <= 0) {
                valid = false;
            }
        } else if (token.kind == Sunlight) {
            if (!is_adjacent(gamestate.player_positions.rooms[player]->room, room)  &&
                    gamestate.player_positions.rooms[player]->room != room) {
                valid = false;
            } else if (gamestate.players[player].num_light <= 0) {
                valid = false;
            }
        } else if (token.kind == HolyWater) {
            if (!is_adjacent(gamestate.player_positions.rooms[player]->room, room) &&
                    gamestate.player_positions.rooms[player]->room != room) {
                valid = false;
            } else if (gamestate.players[player].num_water <= 0) {
                valid = false;
            }
        } else if ((token.kind == Player1 && player == 0) ||
                (token.kind == Player2 && player == 1) ||
                (token.kind == Player3 && player == 2) ||
                (token.kind == Player4 && player == 3) ) {
            if (!is_adjacent(gamestate.player_positions.rooms[player]->room, room) &&
                    gamestate.player_positions.rooms[player]->room != room) {
                valid = false;
            }
        } else {
            valid = false;
        }
    } else {
        valid = (token.room == NUM_ROOMS && (token.kind == HolyWater || token.kind == Sunlight));
    }

    return valid;
}

static void display_health(int player_lives, int dracula_lives) {
    printf("Player lives: %d\nDracula lives: %d\n", player_lives, dracula_lives);
}

static void player_bitten(uint8_t player_num) {
    printf("Player %d was bitten!\n", player_num + 1);
}

static enum TokenKind parse_token_kind(const char *str) {
    for (int i = 0; i < NumTokenKinds; i++) {
        if (strcmp(str, token_type_names[i]) == 0) return i;
    }
    printf("Error: Bad token kind (%s)\n", str);
    return NumTokenKinds; // Error value
}

static enum RoomName parse_room_name(const char *str) {
    for (int i = 0; i < NUM_ROOMS + 1; i++) {
        if (strcmp(str, room_names[i]) == 0) return i;
    }
    printf("Error: Bad room name (%s)\n", str);
    return NUM_ROOMS + 1; // Error value
}

/*
 * Format to parse:
 * TokenType Room
 */
static int get_token(struct Token *buffer) {
    size_t nbytes = 32;
    char *buf = malloc(sizeof(char) * nbytes);
    char token_type[16];
    char room_name[16];
    printf("Enter Tokens:\n");
    for (;;) {
        getline(&buf, &nbytes, stdin);
        int nreads = sscanf(buf, "%s %s", token_type, room_name);
        if (nreads < 0) return 0; // No more actions
        if (nreads < 2) {
            printf("Bad token format\n");
            continue;
        }
        enum TokenKind kind = parse_token_kind(token_type);
        enum RoomName name = parse_room_name(room_name);
        if (kind == NumTokenKinds || name > NUM_ROOMS) continue; // parse error(s) already printed
        *buffer = (struct Token){ .kind = kind, .room = name };
        return 1;
    }
}

/**
 * @brief Retrieves the commited action from the player.
 *
 * @returns A the current turn action which conatins the action to do and 
 * which room it will take place in.
 */
static struct Turn player_input(uint8_t player, struct GameState *gamestate) {
    struct Token tokens[MAX_TOKENS];
    int token_count = get_token(tokens);

    bool error = false;
    int action_val = -1;
    int room_val = -1;
    for (int i = 0; i < token_count; i++) {
        if ((tokens[i].kind == Player1 && player == 0) ||
            (tokens[i].kind == Player2 && player == 1) ||
            (tokens[i].kind == Player3 && player == 2) ||
            (tokens[i].kind == Player4 && player == 3)) {
            if (gamestate->player_positions.rooms[player]->room != tokens[i].room) {
                if (action_val != -1) {
                    error = true;
                    break;
                }
                action_val = MOVE;
                room_val = tokens[i].room;
            }
        }
        else if (tokens[i].kind == Sunlight) {
            if (action_val != -1) {
                error = true;
                break;
            }
            action_val = LIGHT;
            room_val = tokens[i].room;
        }
        else if (tokens[i].kind == HolyWater) {
            if (action_val != -1) {
                error = true;
                break;
            }
            action_val = WATER;
            room_val = tokens[i].room;
        }
        else if (tokens[i].kind == Garlic) {
            if (action_val != -1) {
                error = true;
                break;
            }
            action_val = GARLIC;
            room_val = tokens[i].room;
        }
    }
    if (action_val == -1) {
        printf("Ending turn\n");
        action_val = END;
    }
    if (error) {
        printf("Error: Too many actions\n");
        action_val = ACTION_ERROR;
    }

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
    gamestate->player_resting = true;

    for(;;) {
        struct Token tokens[MAX_TOKENS];
        int token_count = get_token(tokens);

        enum Action resource_val = ACTION_ERROR;
        for (int i = 0; i < token_count; i++) {
            if (tokens[i].room == NUM_ROOMS) {
                if (tokens[i].kind == HolyWater) {
                    resource_val = WATER;
                } else if (tokens[i].kind == Sunlight) {
                    resource_val = LIGHT;
                }
            }
        }

        if (resource_val == WATER) {
            if (gamestate->players[player].num_water < MAX_WATER) {
                gamestate->players[player].num_water++;
            }
            gamestate->player_resting = true;
            break;
        } else if (resource_val == LIGHT) {
            if (gamestate->players[player].num_light < MAX_LIGHT) {
                gamestate->players[player].num_light++;
            }
            gamestate->player_resting = true;
            break;
        } else {
            printf("Error: Invalid resource\n");
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
        printf("Error: not adjacent\n");
        return false;
    }
    if (gamestate->players[player].num_water <= 0) {
        printf("Error: no water\n");
        return false;
    }

    if (dracula_is_present(&rooms[room])) {
        gamestate->dracula_health--;
        gamestate->can_bite = false;
        printf("Dracula is here!\n");
        display_health(gamestate->player_health, gamestate->dracula_health);
    } else {
        printf("Dracula is not here\n");
        display_health(gamestate->player_health, gamestate->dracula_health);
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
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room) &&
                gamestate->player_positions.rooms[player]->room != room) {
        printf("Error: room is not adjacent\n");
        return false;
    }
    if (gamestate->players[player].num_light <= 0 ) {
        printf("Error: no light\n");
        return false;
    }

    if (dracula_is_present(&rooms[room])) {
        printf("Dracula is here!\n");
        display_health(gamestate->player_health, gamestate->dracula_health);
    } else {
        printf("Dracula not is here\n");
        display_health(gamestate->player_health, gamestate->dracula_health);
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
        printf("Error: not adjacent\n");
        return false;
    }
    if (gamestate->garlic <= 0) {
        printf("Error: no garlic\n");
        return false;
    }

    if (dracula_is_present(&rooms[room])) {
        printf("Dracula is here!\n");
        display_health(gamestate->player_health, gamestate->dracula_health);
    } else {
        printf("Dracula is not here\n");
        display_health(gamestate->player_health, gamestate->dracula_health);
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
static bool player_move(uint8_t player, struct GameState *gamestate, enum RoomName room, bool already_moved) {
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room)) {
        printf("Error: not adjacent!\n");
        return false;
    }
    if (already_moved) {
        printf("Error: player has already moved\n");
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

    bool player_moved = false;
    bool garlic_thrown = false;
    bool turn_over = false;
    for (;;) {
        struct Turn turn = player_input(player, gamestate);
        if (turn.action == ACTION_ERROR) continue;
        if (turn.action == END) {
            // Rest occurs when no action has been done
            if (!player_moved && !garlic_thrown && !turn_over) { 
                player_rest(player, gamestate);
            }
            return;
        }
        if (turn_over) continue;
        if (turn.action == WATER && throw_water(player, gamestate, turn.room_name)) turn_over = true;
        if (turn.action == LIGHT && create_light(player, gamestate, turn.room_name)) turn_over = true;
        if (turn.action == GARLIC && throw_garlic(player, gamestate, turn.room_name)) garlic_thrown = true;
        if (turn.action == MOVE && player_move(player, gamestate, turn.room_name, player_moved)) player_moved = true;
    }
}

static void print_room_buffer(const struct RoomBuffer buf) {
    for (int i = 0; i < buf.length; i++) {
        printf("%s\n", room_names[buf.rooms[i]->room]);
    }
    printf("\n");
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

    printf("Player positions:\n");
    print_room_buffer(gamestate->player_positions);

    for (uint8_t i = 0; i < NUM_PLAYERS; i++) {
        gamestate->cur_player = i;

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
        for (uint8_t i = 0; i < NUM_PLAYERS; i++) {
            for (uint8_t j = 0; j < bites.length; j++) {
                if (gamestate->player_positions.rooms[i]->room == bites.rooms[j]->room) {
                    gamestate->players[i].turn_skipped = true;
                    if (gamestate->player_health > 0) { 
                        gamestate->player_health--;
                    }

                    // Players lose one water per bite
                    if (gamestate->players[i].num_water > 0) {
                        gamestate->players[i].num_water--;   
                    }

                    player_bitten(i);
                    display_health(gamestate->player_health, gamestate->dracula_health);
                    break;
                }               
            }
        }
    }
    gamestate->can_bite = true; 
    gamestate->sunlights_from.length = 0;
    gamestate->sunlights_to.length = 0;
}
