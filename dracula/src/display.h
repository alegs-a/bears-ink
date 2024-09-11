#ifndef DISPLAY_H
#define DISPLAY_H

#include <stdint.h>
#include <stdbool.h>

#define DISPLAY_ROW_MIN 0
#define DISPLAY_ROW_MAX 7
#define DISPLAY_COL_MIN 0
#define DISPLAY_COL_MAX 127

/**
 * @brief Initialises the display driver.
 * @returns Zero if the display driver initialised successfully.
 */
int display_init();

/**
 * @brief Reset the display.
 * @returns Zero on success or an error number of failure.
 */
int display_reset();

/**
 * @brief Make the display sleep.
 */
void display_sleep();

/**
 * @brief Wake up the display from sleeping.
 */
void display_wake();

/**
 * @brief Set the display contrast level.
 * @param level From 0 for lowest constrast to 255 for highest constrast.
 */
void display_set_constrast(uint8_t level);

/**
 * @brief Invert all the pixels on the display.
 * @param inverted If the pixels are inverted.
 */
void display_invert(bool inverted);

/**
 * @brief Clear the display.
 * @param data The byte to set each byte of the display to.
 */
void display_clear(unsigned char data);

void display_set_contrast(uint8_t level);

int display_data(uint8_t *data, int length);

int display_set_bounds(uint8_t column_begin, uint8_t column_end, uint8_t row_begin,
        uint8_t row_end);

/**
 * @brief Write pixel data to the display.
 * 
 * The display has 8 rows. Each row has 128 columns. Each row is 8 pixels tall.
 * Each of the 8 bits in a byte written to the display sets the 8 pixel states
 * from LSB to MSH at a given row and column.
 * 
 * @param row_begin The first row to write to, from 0 to 7.
 * @param column_begin The forst column to write to from 0 to 127.
 * @param n The number of bytes to write.
 * @param data Pointer to the buffer to write.
 */
int display_write(uint8_t column_begin, uint8_t column_end, uint8_t row_begin,
        uint8_t row_end, uint8_t *data, unsigned int n);

#endif // DISPLAY_H
