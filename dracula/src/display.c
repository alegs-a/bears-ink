#include "display.h"

#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>

// Thread instance running the main lissajous thread.
static K_THREAD_DEFINE(display, DISPLAY_THREAD_STACK_SIZE,
    display_main, NULL, NULL, NULL, DISPLAY_THREAD_PRIORITY, 0, 0);

// Defined and initialised be the above macro.
extern const k_tid_t display_thread_id;

bool display_init()
{
    return false;
}

void display_main(void *, void *, void *)
{
    for (;;) {
        k_msleep(1000);
    }
}
