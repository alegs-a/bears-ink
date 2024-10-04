#include "buzzer.h"

#include <zephyr/device.h>
#include <zephyr/devicetree.h>
#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>

#include <zephyr/drivers/gpio.h>

void buzzer_main(void *, void *, void *);

static K_THREAD_DEFINE(buzzer, BUZZER_THREAD_STACK_SIZE,
    buzzer_main, NULL, NULL, NULL, BUZZER_THREAD_PRIORITY, 0, 0);

// Defined and initialised be the above macro.
extern const k_tid_t display_thread_id;

static const struct gpio_dt_spec buzzer_pin = GPIO_DT_SPEC_GET(DT_NODELABEL(rfid_buzzer), gpios);

struct buzzer_queue_item {
    void *fifo_reserved;
    enum BuzzKind kind;
};

K_FIFO_DEFINE(buzzer_queue);
K_MEM_SLAB_DEFINE(buzzer_slab, (sizeof(struct buzzer_queue_item) * 8), 1, sizeof(void*));

void buzzer_send(enum BuzzKind kind)
{
    struct buzzer_queue_item *item;
    int error = k_mem_slab_alloc(&buzzer_slab, (void**)&item, K_NO_WAIT);
    if (error != 0) {
        // We don't want to wait for the buzzer thread; it's not that important
        return;
    }

    item->kind = kind;
    k_fifo_put(&buzzer_queue, item);
}

void buzzer_main(void *, void *, void *)
{
    gpio_pin_configure_dt(&buzzer_pin, GPIO_OUTPUT_INACTIVE);

    for (;;) {
        struct buzzer_queue_item *item = k_fifo_get(&buzzer_queue, K_FOREVER);

        switch (item->kind) {
        case READ_OK:
            // BEEEEP
            gpio_pin_set_dt(&buzzer_pin, 1);
            k_msleep(500);
            gpio_pin_set_dt(&buzzer_pin, 0);
            break;
        case READ_ERROR:
            // BI-BIP
            gpio_pin_set_dt(&buzzer_pin, 1);
            k_msleep(50);
            gpio_pin_set_dt(&buzzer_pin, 0);
            k_msleep(50);
            gpio_pin_set_dt(&buzzer_pin, 1);
            k_msleep(50);
            gpio_pin_set_dt(&buzzer_pin, 0);
            break;
        }

        k_mem_slab_free(&buzzer_slab, item);
        k_msleep(100);
    }
}
