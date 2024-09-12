#ifndef DISPLAY_H
#define DISPLAY_H

#include <stdint.h>
#include <stdbool.h>

// The pin configuration is the following:
// - Data In (Blue) -> PA_7 (pin A6) configured as SPI1_MOSI
// - Clock (Yellow) -> PA_1 (pin A1) configured as SPI1_SCK
// - Chip Select (Orange) -> PB_0 (pin D3) configured as SPI_NSS (logical not slave select)
// - Data Command (Green) -> PA_4 (pin A3) configured as GPIO
// - Reset (White) -> PA_0 (pin A0) configured as GPIO

// The width of the display in pixels.
#define DISPLAY_WIDTH 128

// THe height of the display in pixels. Divided into 8 rows.
#define DISPLAY_HEIGHT 64

// The stack size of the dracula thread.
#define DISPLAY_THREAD_STACK_SIZE 2048

// The thread priority of the dracula logic.
#define DISPLAY_THREAD_PRIORITY 5

/**
 * @brief Initialises the display driver.
 * @returns Zero if the display driver initialised successfully.
 */
int display_init();

/**
 * @brief Make the display sleep.
 */
void display_sleep();

/**
 * @brief Wake up the display from sleeping.
 */
void display_wake();

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

/**
 * @brief Set the contrast of the display.
 * 
 * @param level The contrast level from 0 to 255.
 */
void display_set_contrast(uint8_t level);

/**
 * @brief Write pixel data to the display.
 * 
 * The display has 8 rows. Each row has 128 columns. Each row is 8 pixels tall.
 * Each of the 8 bits in a byte written to the display sets the 8 pixel states
 * from LSB to MSH at a given row and column.
 * 
 * @param column_begin The starting column from 0 to 127 inclusive.
 * @param column_end The end column from 0 to 127 inclusive.
 * @param row_begin The starting row from 0 to 7 inclusive.
 * @param row_end The end row from 0 to 7 inclusive.
 * @param data Pointer to the buffer to write.
 * @param n The number of bytes to write.
 * 
 * @returns Zero on success or a non-zero error number on failure.
 */
int display_write(uint8_t column_begin, uint8_t column_end, uint8_t row_begin,
        uint8_t row_end, uint8_t *data, unsigned int n);

/**
 * @brief A structure containing an image to display.
 */
struct Image {

    /// The width of the display image.
    unsigned char width;

    /// The height of the display image.
    unsigned char height;

    /// The size of the image data buffer.
    unsigned int size;

    /// The image data buffer.
    unsigned char *buffer;
};

/**
 * @brief Display an image.
 * 
 * @note This function is asynchronous.
 * 
 * @param image The image to display.
 * @param x The x coordinate of the image.
 * @param y The y coordinate of the image.
 * 
 * @returns Zero on success or a non-zero error number on failure.
 */
int display_image(struct Image *image, unsigned int x, unsigned int y);

/**
 * @brief Simple test routine for the display.
 */
void display_test();

#endif // DISPLAY_H
