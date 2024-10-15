#ifndef LED_H
#define LED_H

// Number of leds on the strip.
#define MAX_LEDS 200

/**
 * @brief Initialise the led strip.
 */
int led_init();

/**
 * @brief Write the value of a led.
 * 
 * @param led The led to write.
 * @param r The red value from 0 to 255.
 * @param g The green value from 0 to 255.
 * @param b The blue value from 0 to 255.
 */
void led_write(unsigned int led, unsigned char r, unsigned char g, unsigned char b);

/**
 * @brief Update the led strip.
 */
void led_update();

#endif // LED_H
