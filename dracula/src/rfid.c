#include "rfid.h"

#include <zephyr/device.h>
#include <zephyr/devicetree.h>
#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>

#include <zephyr/drivers/i2c.h>

// Thread running the rfid driver.
// static K_THREAD_DEFINE(rfid, RFID_THREAD_STACK_SIZE,
//     rfid_main, NULL, NULL, NULL, RFID_THREAD_PRIORITY, 0, 0);

// Defined and initialised by the above macro.
// extern const k_tid_t rfid_thread_id;

const struct device *const i2c1_dev = DEVICE_DT_GET(DT_ALIAS(rfid_i2c));

bool rfid_init()
{
    if (!device_is_ready(i2c1_dev)) {
        printk("I2C bus not ready\n");
        return -ENODEV;
    }
    uint8_t version = 0;
    printk("Reading version\n");
    i2c_reg_read_byte(i2c1_dev, 0x2c, 0x37, &version);
    printk("MRFC522 version: %x\n", version);
    i2c_reg_read_byte(i2c1_dev, 0x2c, 0x07, &version);
    printk("MRFC522 status: %x\n", version);
    return version != 0;
}

void rfid_main(void *, void *, void *)
{
    for (;;) {
        k_msleep(1000);
    }
}
