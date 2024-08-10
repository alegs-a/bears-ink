#include "rfid.h"

#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>

// Thread running the rfid driver.
static K_THREAD_DEFINE(rfid, RFID_THREAD_STACK_SIZE,
    rfid_main, NULL, NULL, NULL, RFID_THREAD_PRIORITY, 0, 0);

// Defined and initialised by the above macro.
extern const k_tid_t rfid_thread_id;

bool rfid_init()
{
    return false;
}

void rfid_main(void *, void *, void *)
{
    for (;;) {
        k_msleep(1000);
    }
}
