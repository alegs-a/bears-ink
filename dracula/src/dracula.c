#include "dracula.h"
#include "ai.h"
#include "room.h"
#include "rfid.h"
#include "ui.h"
#include "led.h"

#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>
#include <zephyr/drivers/gpio.h>

static const struct gpio_dt_spec button =
    GPIO_DT_SPEC_GET(DT_NODELABEL(confirm_button), gpios);

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

#ifdef DEBUG
char *room_names[NUM_ROOMS] = {
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
};
#endif

// Game state with its corresponding mutex lock
struct GameState gamestate;
K_MUTEX_DEFINE(gamestateMutex);

static void full_dracula_turn(struct GameState *gamestate);
static void full_players_turn(struct GameState *gamestate);

/**
 * @brief Runs the game.
 */
void dracula_main() {
    // Initialise the button
    if (!gpio_is_ready_dt(&button)) {
        printk("pushbutton not ready\n");
    }
    int error = gpio_pin_configure_dt(&button, GPIO_INPUT);
    if (error) {
        printk("gpio pin configure fail\n");
    }

    // Initialise the AI
    dracula_setup();
  
    // Initialise gamestate
    k_mutex_lock(&gamestateMutex, K_FOREVER);
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
    k_mutex_unlock(&gamestateMutex);

    //Initialise LED resource count
    for (uint8_t i = 0; i < NUM_PLAYERS; i++) {
        for (uint8_t j = 0; j < MAX_LIGHT; j++) {
            uint8_t idx = i * (MAX_LIGHT + MAX_WATER) + j;
            led_write(idx, 255, 255, 0);
            led_write(idx + MAX_LIGHT, 0, 255, 255);
        }
    }
    led_update();

    // Main Game Loop
    for (;;) {
        full_players_turn(&gamestate);
        full_dracula_turn(&gamestate);

        //Handle game ending
        if (gamestate.player_health <= 0) {
            //TODO handle game loss
            // printf("Dracula wins!");
            break;
        } else if (gamestate.dracula_health <= 0) {
            //TODO handle game win
            // printf("Player's win!");
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
 * @brief Check if a given token placed on a specific RFID provides a valid 
 *        action.
 * 
 * @param token The token that will be checked.
 * 
 * @returns True if the action is valid, false otherwise.
 */
bool token_valid(struct Token token) {
    k_mutex_lock(&gamestateMutex, K_FOREVER);
    uint8_t player = gamestate.cur_player;
    enum RoomName room = token.room;
    bool valid = true;

    if (!gamestate.player_resting) {
        if (token.room == NUM_ROOMS) {
            // Token is on resource gain
            valid = false;
        } else if (token.kind == Garlic) {
            // Check if garlic token is valid
            if (!is_adjacent(gamestate.player_positions.rooms[player]->room, room) &&
                gamestate.player_positions.rooms[player]->room != room) {
                valid = false;
            } else if (gamestate.garlic <= 0) {
                valid = false;
            }
        } else if (token.kind == Sunlight) {
            // Check if sunlight token is valid
            if (!is_adjacent(gamestate.player_positions.rooms[player]->room, room)  &&
                    gamestate.player_positions.rooms[player]->room != room) {
                valid = false;
            } else if (gamestate.players[player].num_light <= 0) {
                valid = false;
            }
        } else if (token.kind == HolyWater) {
            // Check if water token is valid
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
            // Check if player token (movement) is valid
            if (!is_adjacent(gamestate.player_positions.rooms[player]->room, room) &&
                    gamestate.player_positions.rooms[player]->room != room) {
                valid = false;
            }
        } else {
            // Invalid token
            valid = false;
        }
    } else {
        // If resting, only water and sunlight can be put on gain resource reader
        valid = (token.room == NUM_ROOMS && (token.kind == HolyWater || token.kind == Sunlight));
    }

    k_mutex_unlock(&gamestateMutex);
    return valid;
}

/**
 * @brief Retrieves the commited action from the player.
 *
 * @returns A the current turn action which conatins the action to do and 
 * which room it will take place in.
 */
static struct Turn player_input(uint8_t player, struct GameState *gamestate) {
    //Wait until confirmation button
    while (gpio_pin_get_dt(&button)) {
        // Wait for button to be unpressed, before being pressed again
        k_msleep(100);
    }
    k_msleep(100); // Debounce
    printk("Waiting for turn confirmation...");
    for(;;) {
        k_msleep(100);
        if (gpio_pin_get_dt(&button)) {
            printk(" Button!\n");
            break;
        }
    }

    // Read tokens from readers
    struct Token tokens[MAX_TOKENS];
    int token_count = rfid_get_tokens(tokens);

    // Convert tokens to actions the game can read
    k_mutex_lock(&gamestateMutex, K_FOREVER);
    bool error = false;
    int action_val = -1;
    int room_val = -1;
    for (int i = 0; i < token_count; i++) {
        if ((tokens[i].kind == Player1 && player == 0) ||
            (tokens[i].kind == Player2 && player == 1) ||
            (tokens[i].kind == Player3 && player == 2) ||
            (tokens[i].kind == Player4 && player == 3)) {
            if (gamestate->player_positions.rooms[player]->room != tokens[i].room) {
                // Create an error if an action has be done already
                if (action_val != -1) {
                    error = true;
                    break;
                }
                action_val = MOVE;
                room_val = tokens[i].room;
            }
        }
        else if (tokens[i].kind == Sunlight) {
            // Create an error if an action has be done already
            if (action_val != -1) {
                error = true;
                break;
            }
            action_val = LIGHT;
            room_val = tokens[i].room;
        }
        else if (tokens[i].kind == HolyWater) {
            // Create an error if an action has be done already
            if (action_val != -1) {
                error = true;
                break;
            }
            action_val = WATER;
            room_val = tokens[i].room;
        }
        else if (tokens[i].kind == Garlic) {
            // Create an error if an action has be done already
            if (action_val != -1) {
                error = true;
                break;
            }
            action_val = GARLIC;
            room_val = tokens[i].room;
        }
    }

    // The player chose to end their turn
    if (action_val == -1) {
        action_val = END;
    }

    // Display an error if there is an error
    if (error) {
        display_clear(0x00);
        err_too_many_actions();
        display_health(gamestate->player_health, gamestate->dracula_health);
        action_val = ACTION_ERROR;
    } else {
        display_clear(0x00);
        mes_valid_action();
        display_health(gamestate->player_health, gamestate->dracula_health);
    }

    k_mutex_unlock(&gamestateMutex);
    return (struct Turn){.action=action_val, .room_name=room_val};
} 

/**
 * @brief Updates the sunligh count on the LED strip.
 * 
 * @param player The current player index.
 * @param num The number of sunlights the player has.
 * @param remove True if the LED is to be turned off.
 */
static void update_light_led(uint8_t player, uint8_t num, bool remove) {
    uint8_t i = player * (MAX_LIGHT + MAX_WATER) + num - 1;
    if (remove) {
        led_write(i, 0, 0, 0);
    } else {
        led_write(i, 255, 255, 0);
    }
    led_update();
}

/**
 * @brief Updates the holy water count on the LED strip.
 * 
 * @param player The current player index.
 * @param num The number of holy waters the player has.
 * @param remove True if the LED is to be turned off.
 */
static void update_water_led(uint8_t player, uint8_t num, bool remove) {
    uint8_t idx = player * (MAX_LIGHT + MAX_WATER) + MAX_LIGHT + num - 1;
    if (remove) {
        led_write(idx, 0, 0, 0);
    } else {
        led_write(idx, 0, 0, 255);
    }
    led_update();
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
    k_mutex_lock(&gamestateMutex, K_FOREVER);
    gamestate->player_resting = true;
    k_mutex_unlock(&gamestateMutex);

    for(;;) {
        //Wait until confirmation button
        while (gpio_pin_get_dt(&button)) {
            // Wait for button to be unpressed, before being pressed again
            k_msleep(100);
        }
        k_msleep(100); // Debounce
        printk("Waiting for rest confirmation...");
        for(;;) {
            k_msleep(100);
            if (gpio_pin_get_dt(&button)) {
                printk(" Button!\n");
                break;
            }
        }

        // Read tokens from readerss
        k_mutex_lock(&gamestateMutex, K_FOREVER);
        struct Token tokens[MAX_TOKENS];
        int token_count = rfid_get_tokens(tokens);

        // Read resources from the token
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

        // Add the resource to the current player
        if (resource_val == WATER) {
            if (gamestate->players[player].num_water < MAX_WATER) {
                gamestate->players[player].num_water++;
                update_water_led(player, gamestate->players[player].num_water, false);
            }
            gamestate->player_resting = true;
            k_mutex_unlock(&gamestateMutex);
            break;
        } else if (resource_val == LIGHT) {
            if (gamestate->players[player].num_light < MAX_LIGHT) {
                gamestate->players[player].num_light++;
                update_light_led(player, gamestate->players[player].num_light, false);
            }
            gamestate->player_resting = true;
            k_mutex_unlock(&gamestateMutex);
            break;
        } else {
            display_clear(0x00);
            err_invalid_resource();
            display_health(gamestate->player_health, gamestate->dracula_health);
        }
        k_mutex_unlock(&gamestateMutex);
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
    k_mutex_lock(&gamestateMutex, K_FOREVER);

    // Check if action is valid
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room) &&
            gamestate->player_positions.rooms[player]->room != room) {
        display_clear(0x00);
        err_not_adjacent();
        display_health(gamestate->player_health, gamestate->dracula_health);
        return false;
    }
    if (gamestate->players[player].num_water <= 0) {
        display_clear(0x00);
        err_no_water();
        display_health(gamestate->player_health, gamestate->dracula_health);
        return false;
    }

    // Display if Dracula is there
    if (dracula_is_present(&rooms[room])) {
        gamestate->dracula_health--;
        gamestate->can_bite = false;
        display_clear(0x00);
        mes_dracula();
        display_health(gamestate->player_health, gamestate->dracula_health);
    } else {
        display_clear(0x00);
        mes_no_dracula();
        display_health(gamestate->player_health, gamestate->dracula_health);
    }

    // Update game state
    update_water_led(player, gamestate->players[player].num_water, true);
    gamestate->players[player].num_water--;
    k_mutex_unlock(&gamestateMutex);
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
    k_mutex_lock(&gamestateMutex, K_FOREVER);

    // Check if action is valid
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room) &&
                gamestate->player_positions.rooms[player]->room != room) {
        display_clear(0x00);
        err_not_adjacent();
        display_health(gamestate->player_health, gamestate->dracula_health);
        return false;
    }
    if (gamestate->players[player].num_light <= 0 ) {
        display_clear(0x00);
        err_no_light();
        display_health(gamestate->player_health, gamestate->dracula_health);
        return false;
    }

    // Display if Dracula is there
    if (dracula_is_present(&rooms[room])) {
        display_clear(0x00);
        mes_dracula();
        display_health(gamestate->player_health, gamestate->dracula_health);
    } else {
        display_clear(0x00);
        mes_no_dracula();
        display_health(gamestate->player_health, gamestate->dracula_health);
    }

    // Update game state
    add_with_duplicate(&(gamestate->sunlights_from), gamestate->player_positions.rooms[player]);
    add_with_duplicate(&(gamestate->sunlights_to), &rooms[room]);
    update_light_led(player, gamestate->players[player].num_light, true);
    gamestate->players[player].num_light--;
    k_mutex_unlock(&gamestateMutex);
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
    k_mutex_lock(&gamestateMutex, K_FOREVER);

    // Check if action is valid
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room) &&
            gamestate->player_positions.rooms[player]->room != room) {
        display_clear(0x00);
        err_not_adjacent();
        display_health(gamestate->player_health, gamestate->dracula_health);
        return false;
    }
    if (gamestate->garlic <= 0) {
        display_clear(0x00);
        err_no_garlic();
        display_health(gamestate->player_health, gamestate->dracula_health);
        return false;
    }

    // Display if Dracula is there
    if (dracula_is_present(&rooms[room])) {
        display_clear(0x00);
        mes_dracula();
        display_health(gamestate->player_health, gamestate->dracula_health);
    } else {
        display_clear(0x00);
        mes_no_dracula();
        display_health(gamestate->player_health, gamestate->dracula_health);
    }

    // Update game state
    gamestate->garlic--;
    k_mutex_unlock(&gamestateMutex);
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
    k_mutex_lock(&gamestateMutex, K_FOREVER);

    // Check if action is valid
    if (!is_adjacent(gamestate->player_positions.rooms[player]->room, room)) {
        display_clear(0x00);
        err_not_adjacent();
        display_health(gamestate->player_health, gamestate->dracula_health);
        return false;
    }
    if (already_moved) {
        display_clear(0x00);
        err_already_moved();
        display_health(gamestate->player_health, gamestate->dracula_health);
        return false;
    }

    // Update game state
    gamestate->player_positions.rooms[player] = &rooms[room];
    k_mutex_unlock(&gamestateMutex);
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
    k_mutex_lock(&gamestateMutex, K_FOREVER);

    // Initialise player turn
     if (!gamestate->players[player].can_bite) {
        gamestate->players[player].can_bite = true;
    }
    if (gamestate->players[player].turn_skipped) {
        gamestate->players[player].turn_skipped = false;
        gamestate->players[player].can_bite = false;
        return;
    }
    k_mutex_unlock(&gamestateMutex);
    display_clear(0x00);
    err_not_your_turn(player);
    display_health(gamestate->player_health, gamestate->dracula_health);

    bool player_moved = false;
    bool garlic_thrown = false;
    // Repeat actions until the turn ends
    for (;;) {
        struct Turn turn = player_input(player, gamestate);
        if (turn.action == ACTION_ERROR) { 
            continue;
        } else if (turn.action == END) {
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
        else if (turn.action == MOVE && player_move(player, gamestate, turn.room_name, player_moved)) {
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
    k_mutex_lock(&gamestateMutex, K_FOREVER);
    gamestate->garlic = MAX_GARLIC;
    gamestate->can_bite_player_positions.length = 0;
    for (uint8_t i = 0; i < NUM_PLAYERS; i++) {
        // Run player turn
        gamestate->cur_player = i;
        k_mutex_unlock(&gamestateMutex);
        player_turn(i, gamestate);
        k_mutex_lock(&gamestateMutex, K_FOREVER);

        // Update game state
        if (gamestate->players[i].can_bite) {
            add_with_duplicate(&(gamestate->can_bite_player_positions), gamestate->player_positions.rooms[i]);
        }

        // Checks if the game is over
        if (gamestate->dracula_health <= 0) {
            break;
        } 
        k_mutex_unlock(&gamestateMutex);
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
    k_mutex_lock(&gamestateMutex, K_FOREVER);
    if (bites.length != 0) {
        for (uint8_t i = 0; i < NUM_PLAYERS; i++) {
            for (uint8_t j = 0; j < bites.length; j++) {
                if (gamestate->player_positions.rooms[i]->room == bites.rooms[j]->room) {
                    // Player takes damage
                    gamestate->players[i].turn_skipped = true;
                    if (gamestate->player_health > 0) { 
                        gamestate->player_health--;
                    }

                    // Players lose one water per bite
                    if (gamestate->players[i].num_water > 0) {
                        update_water_led(i, gamestate->players[i].num_water, true);
                        gamestate->players[i].num_water--;   
                    }

                    // Draw which player has been bitten
                    display_clear(0x00);
                    mes_player_bitten(i);
                    display_health(gamestate->player_health, gamestate->dracula_health);
                    k_msleep(3000);
                    break;
                }               
            }
        }
    }    
    // Update game state
    gamestate->can_bite = true; 
    gamestate->sunlights_from.length = 0;
    gamestate->sunlights_to.length = 0;
    k_mutex_unlock(&gamestateMutex);
}
