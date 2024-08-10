#include <stdio.h>
#include <math.h>

#include <zephyr/kernel.h>
#include <zephyr/init.h>

#include "rfid.h"
#include "display.h"
#include "dracula.h"

int init()
{
    printk("Hello World\n");

    if (!rfid_init() || !display_init()) {
        printk("initialisation failed");
    }

    return 0;
}

SYS_INIT(init, APPLICATION, 1);

int main()
{
    // Yield to worker threads.
    for (;;) {
        k_msleep(1000);
    }

    return 0;
}
