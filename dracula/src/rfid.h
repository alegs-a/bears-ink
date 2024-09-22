#ifndef RFID_H
#define RFID_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

// The stack size of the dracula thread.
#define RFID_THREAD_STACK_SIZE 2048

// The thread priority of the dracula logic.
#define RFID_THREAD_PRIORITY 5

enum TokenKind {
    Player1,
    Player2,
    Player3,
    Player4,
    Garlic,
    Sunlight,
    HolyWater,
};

/**
 * @brief The rfid thread handling I/O from rfid readers.
 */
void rfid_main(void *, void *, void *);

#ifdef __cplusplus
}
#endif

#endif // RFID_H
