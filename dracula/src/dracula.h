#ifndef DRACULA_H
#define DRACULA_H

#include <stdbool.h>

// The stack size of the dracula thread.
#define DRACULA_THREAD_STACK_SIZE 2048

// The thread priority of the dracula logic.
#define DRACULA_THREAD_PRIORITY 5

bool dracula_init();

void dracula_main(void *, void *, void *);

#endif // DRACULA_H
