#ifndef RFID_H
#define RFID_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <zephyr/kernel.h>
#include "room.h"

// The stack size of the rfid thread.
#define RFID_THREAD_STACK_SIZE 2048

// The thread priority of the thread logic.
#define RFID_THREAD_PRIORITY 5

// Defined in main.c
extern k_tid_t rfid_thread_id;

// The maximum number of tokens that should be tracked by the driver
#define MAX_TOKENS 64

enum TokenKind {
    Player1,
    Player2,
    Player3,
    Player4,
    Garlic,
    Sunlight,
    HolyWater,
};

struct Token {
    enum TokenKind kind;
    enum RoomName room;
};

/**
 * @brief Get a list of the currently tracked tokens and their positions
 *
 * Tokens will be output in an arbitrary order.
 *
 * @param[out] tokens
 * @parblock
 * A pointer to a list of tokens to be written to.
 *
 * The list must have space for at least MAX_TOKENS tokens, otherwise a buffer
 * overflow may occur.
 * @endparblock
 *
 * @return The number of tokens written
 */
int rfid_get_tokens(struct Token *tokens);

void rfid_onestep();

/**
 * @brief The rfid thread handling I/O from rfid readers.
 */
void rfid_main(void *, void *, void *);

#ifdef __cplusplus
}
#endif

#endif // RFID_H
