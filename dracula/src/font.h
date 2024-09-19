#ifndef FONT_H
#define FONT_H

#include "display.h"

/**
 * @brief Display string.
 * 
 * @param string The string to display.
 * @param x The x coordinate of the string.
 * @param y The y coordinate of the string.
 * 
 * @returns If the string was successfully displayed.
 */
int display_string(const char *string, unsigned int x, unsigned int y);

#endif // FONT_H
