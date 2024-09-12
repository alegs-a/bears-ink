#include <stdio.h>
#include <math.h>

#include <zephyr/kernel.h>
#include <zephyr/init.h>

#include "font.h"
#include "rfid.h"
#include "display.h"
#include "dracula.h"

int main()
{
    printk("Dracula!\n");

    int error = display_init();
    if (error) {
        printk("display init failed with %i\n", error);
    }

    // error = rfid_init();
    // if (error) {
    //     printk("rfid init failed with %i\n", error);
    // }

    // if (error) {
    //     for (;;) {
    //         k_msleep(1000);
    //         printk("error\n");
    //     }
    // }

    display_clear(0x00);
    display_string("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", 0, 0);

    // Yield to worker threads.
    for (;;) {
        k_msleep(1000);
    }

    return 0;
}
