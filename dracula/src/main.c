#include <stdio.h>
#include <math.h>

#include <zephyr/kernel.h>
#include <zephyr/init.h>

#include "ui.h"
#include "font.h"
#include "display.h"
#include "dracula.h"

int main()
{
    printk("Dracula!\n");

    int error = display_init();
    if (error) {
        printk("display init failed with %i\n", error);
    }


    display_clear(0x00);
    ui_splash();
    // display_string("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", 0, 0);

    printk("Initialisation done.\n");

    dracula_main();

    return 0;
}
