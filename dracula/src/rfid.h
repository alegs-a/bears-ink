#ifndef RFID_H
#define RFID_H

#include <stdbool.h>

// The stack size of the dracula thread.
#define RFID_THREAD_STACK_SIZE 2048

// The thread priority of the dracula logic.
#define RFID_THREAD_PRIORITY 5

/**
 * @brief Initialise the rfid driver.
 */
bool rfid_init();

/**
 * @brief The rfid thread handling I/O from rfid readers.
 */
void rfid_main(void *, void *, void *);

#endif // RFID_H
