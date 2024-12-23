#ifndef BUZZER_H
#define BUZZER_H

// The stack size of the buzzer thread.
#define BUZZER_THREAD_STACK_SIZE 1024

// The thread priority of the thread logic.
#define BUZZER_THREAD_PRIORITY 9

enum BuzzKind {
    STARTUP,
    READ_OK,
    READ_ERROR,
    TURN_OK,
    TURN_ERROR,
    TURN_END,
};

void buzzer_send(enum BuzzKind kind);

#endif // BUZZER_H
