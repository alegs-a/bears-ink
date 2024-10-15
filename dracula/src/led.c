#include "led.h"

#include <zephyr/kernel.h>
#include <zephyr/sys/printk.h>
#include <zephyr/drivers/gpio.h>

// static const struct gpio_dt_spec data_pin = GPIO_DT_SPEC_GET(DT_NODELABEL(led_strip), gpios);

typedef struct __attribute__((packed)) {
    unsigned char r;
    unsigned char g;
    unsigned char b;
} Rgb;

static Rgb leds[3 * MAX_LEDS];

int led_init()
{
    for (int i = 0; i < MAX_LEDS; i++) {
        leds[i].r = 0;
        leds[i].g = 0;
        leds[i].b = 0;
    }

    return 0;
}

// datasheet: https://cdn-shop.adafruit.com/datasheets/WS2812.pdf

// Assuming 80MHz clock

// 1 / (80Mhz / 1e6 * 1e9) = 12.5ns per cycle
// delay / ns per cycle = number of nop

#define DELAY_350NS() \
    __asm__ ( \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop;" \
    )

#define DELAY_700 \
    __asm__ ( \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop;" \
    )

#define DELAY_800 \
    __asm__ ( \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop;" \
    )

#define DELAY_600 \
    __asm__ ( \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; nop; " \
        "nop; nop; nop; nop; nop; nop; nop; nop; nop;" \
    )

/**
 * @brief 
 */
void led_write(unsigned int led, unsigned char r, unsigned char g,
        unsigned char b)
{
    if (led >= MAX_LEDS)
        return;
    
    leds[led].r = r;
    leds[led].g = g;
    leds[led].b = b;
}

/**
 * @brief Sends a bit to the strip in non-zero return line coding.
 * 
 * @param bit The bit to send.
 */
__attribute__((always_inline)) inline void send_bit(unsigned char bit)
{
    // Setting the pin values must be done within 12 cycles.
    // if (bit) {
    //     gpio_pin_set_dt(&data_pin, 1);
    //     DELAY_700NS();
    //     gpio_pin_set_dt(&data_pin, 0);
    //     DELAY_600NS();
    // }
    // else {
    //     gpio_pin_set_dt(&data_pin, 1);
    //     DELAY_350NS();
    //     gpio_pin_set_dt(&data_pin, 0);
    //     DELAY_800NS();
    // }
};

/**
 * @brief Sends a byte to the LED strip.
 * @param byte The byte to send.
 */
__attribute__((always_inline)) inline void send_byte(unsigned char byte)
{
    // Most to least significant bits sent.
    for (int i = 0; i < 8; i++) {
        send_bit((byte >> i) | 0b1);
    }
}

void led_update()
{
    for (int i = 0; i < MAX_LEDS; i++) {
        send_byte(leds[i].g);
        send_byte(leds[i].r);
        send_byte(leds[i].b);       
    }
}
