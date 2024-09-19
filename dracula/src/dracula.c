#include "dracula.h"

#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>

// static K_THREAD_DEFINE(dracula, DRACULA_THREAD_STACK_SIZE,
//     dracula_main, NULL, NULL, NULL, DRACULA_THREAD_PRIORITY, 0, 0);

// Defined and initialised be the above macro.
extern const k_tid_t dracula_thread_id;

static void dracula_turn();
static struct turn player_input();
static bool player_turn(uint8_t player, struct gamestate *gamestate);

bool dracula_init()
{
    return false;
}

void dracula_main(void *, void *, void *)
{
    // TODO handle logo sequence

    // Initialise players
    struct player players[PLAYER_COUNT] = {
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .room=&rooms[NHALL], .turn_skipped=false},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .room=&rooms[NHALL], .turn_skipped=false},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .room=&rooms[NHALL], .turn_skipped=false},
        {.num_water=INIT_WATER, .num_light=INIT_LIGHT, .room=&rooms[NHALL], .turn_skipped=false}
    };

    // Initialise game state
    struct gamestate gamestate = {.player_health=PLAYER_HEALTH, 
        .garlic=MAX_GARLIC, .dracula_health=DRACULA_HEALTH, .can_bite=false, 
        .players=&players[0]};

    for (;;) {
        // Don't forget to change 1 to PLAYER_COUNT after testing
        for (uint8_t i = 0; i < 1; i++) {
            player_turn(i, &gamestate);
        }
        dracula_turn();
    }
    
    //for (;;) {
    //    k_msleep(1000);
    //}
}

/* 
 * Check if a room is adjacent to another room.
 *
 * src: The room which the adjacency list will be checked
 * dst: The room which will be checked
 * 
 * Returns a bool indicating adjacency.
 */
static bool is_adjacent(enum room_name src, enum room_name dst) {
    for (uint8_t i = 0; i < rooms[src].adjacent->length; i++) {
        if (rooms[src].adjacent->rooms[i]->name == dst) {
            return true;
        }    
    }
    return false;
}

/*
 * Receives input from the board to complete a turn.
 *
 * Returns a struct containing the enum values of action and room to take place.
 */
static struct turn player_input() {
    // TODO make input from RFID readers instead of scanf
    //char action[3];
    //printf("Input Action:\n");
    //scanf("%s", &action);
    //uint8_t action_val = atoi(action);
    
    //char room[2];
    //printf("Input Room:\n");
    //scanf("%s", &room);
    //uint8_t room_val = atoi(room);

    return (struct turn){.action=0, .room_name=0};
} 

/*
 * Handles the rest action by allowing players to choose a resource to gain.
 *
 * player: The index of the current player
 * gamestate: A pointer to the current gamestate  
 */
static void player_rest(uint8_t player, struct gamestate *gamestate) {
    for(;;) {
        // TODO make input from RFID readers instead of scanf
        //char resource[3];
        //printf("Choose Resource:\n");
        //scanf("%s", &resource);
        //uint8_t resource_val = atoi(resource); 
        uint8_t resource_val = 1;

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
            //printf("Invalid Resource\n");
        }
    }
}

/*
 */
static void throw_water() {

}

/*
 */
static void create_light() {

}

/*
 */
static void throw_garlic() {

}

/*
 */
static void player_move() {

}

/*
 * Handles the players turn by managing the actions done by players.
 *
 * Returns true if the game has ended.
 */
static bool player_turn(uint8_t player, struct gamestate *gamestate) {
    bool can_rest = true;
    for (;;) {
        struct turn turn = player_input();
        if (turn.action == END) {
            if (can_rest) { // Rest occurs when no actions has been done
                player_rest(player, gamestate);
            }
            break;
        }
        else if (turn.action == WATER) {
            //printf("WATER\n");
            break;
        }
        else if (turn.action == LIGHT) {
            //printf("LIGHT\n");
            break;
        }
        else if (turn.action == GARLIC) {
            //printf("GARLIC\n");
            can_rest = false;
        }
        else if (turn.action == MOVE) {
            //printf("MOVE\n");
            can_rest = false;
        } else {
            //printf("Invalid Action\n");
        }
    }
    return false;
}

/*
 */
static void dracula_turn () {

}