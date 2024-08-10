#ifndef DISPLAY_H
#define DISPLAY_H

#include <stdbool.h>

// The stack size of the dracula thread.
#define DISPLAY_THREAD_STACK_SIZE 2048

// The thread priority of the dracula logic.
#define DISPLAY_THREAD_PRIORITY 5

bool display_init();

void display_main(void *, void *, void *);

#endif // DISPLAY_H
