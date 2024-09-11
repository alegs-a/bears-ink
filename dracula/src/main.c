#include <stdio.h>
#include <math.h>

#include <zephyr/kernel.h>
#include <zephyr/init.h>

#include "rfid.h"
#include "display.h"
#include "dracula.h"

int main()
{
    printk("Hello World\n");

    int error = display_init();
    if (error) {
        printk("display init failed with %i\n", error);
    }

    error = rfid_init();
    if (error) {
        printk("rfid init failed with %i\n", error);
    }

    if (error) {
        for (;;) {
            k_msleep(1000);
            printk("error\n");
        }
    }

    display_clear(0xF0);

    // unsigned char byte = 0x00;
    // display_set_bounds(0, 127, 7, 0);
    // for (int i = 0; i < 128; i++)
    //     display_data(&byte, 1);

    // display_set_contrast(255);

    // Yield to worker threads.
    for (;;) {
        k_msleep(1000);
        // display_clear(0xFF);
    }

    return 0;
}
