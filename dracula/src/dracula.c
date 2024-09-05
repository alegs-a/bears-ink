#include "dracula.h"

#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>

// static K_THREAD_DEFINE(dracula, DRACULA_THREAD_STACK_SIZE,
//     dracula_main, NULL, NULL, NULL, DRACULA_THREAD_PRIORITY, 0, 0);

// Defined and initialised be the above macro.
extern const k_tid_t dracula_thread_id;

bool dracula_init()
{
    return false;
}

void dracula_main(void *, void *, void *)
{
    for (;;) {
        k_msleep(1000);
    }
}
