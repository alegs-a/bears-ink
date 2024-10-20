#include <stdio.h>
#include <math.h>

#include <zephyr/kernel.h>
#include <zephyr/init.h>

#include "buzzer.h"
#include "display.h"
#include "dracula.h"
#include "rfid.h"
#include "ui.h"

K_THREAD_STACK_DEFINE(rfid_stack, RFID_THREAD_STACK_SIZE);
struct k_thread rfid_thread_data;

k_tid_t rfid_thread_id;

int main()
{
    printk("Dracula!\n");

    int error = display_init();
    if (error) {
        printk("display init failed with %i\n", error);
    }

    buzzer_send(STARTUP);
    rfid_thread_id = k_thread_create(&rfid_thread_data, rfid_stack,
                                     K_THREAD_STACK_SIZEOF(rfid_stack),
                                     rfid_main,
                                     NULL, NULL, NULL,
                                     RFID_THREAD_PRIORITY, 0, K_NO_WAIT);
    printk("Initialisation done.\n");

    display_clear(0x00);
    ui_splash();
    // display_string("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", 0, 0);
    dracula_main();

    return 0;
}
