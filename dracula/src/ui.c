#include "ui.h"

int ui_init()
{
    return display_init();
}

unsigned char buffer_dracula[445] = {
    0x07, 0x7E, 0xF8, 0xF8, 0x30, 0x30, 0x20, 0x60, 0x40, 0xC0, 0x80, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x20, 0x60, 0x60, 0xE0, 0xC0,
    0xCE, 0x9C, 0x78, 0xD0, 0xF0, 0xD0, 0x78, 0x9C, 0xCE, 0xC0, 0xE0, 0x60,
    0x60, 0x20, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3E, 0xF8,
    0xF8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x01, 0xFE, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x13, 0x1A, 0x06, 0x07, 0x1A, 0x12,
    0x03, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00,
    0x00, 0x00, 0xF0, 0xE0, 0x60, 0x60, 0x60, 0x60, 0x40, 0xC0, 0x00, 0x00,
    0x00, 0x00, 0x80, 0x80, 0xE0, 0x70, 0x70, 0x30, 0x30, 0x30, 0x60, 0xE0,
    0xC0, 0x00, 0x00, 0x00, 0x80, 0xC0, 0xE0, 0x30, 0x30, 0x30, 0x30, 0x70,
    0xE0, 0xC0, 0x00, 0x00, 0x00, 0x00, 0xF0, 0xF0, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xF0, 0xF0, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0xE0, 0x70, 0x70, 0x30, 0x30, 0x30,
    0x60, 0xE0, 0xC0, 0x00, 0xF0, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0xE0, 0x7F, 0x1F, 0x00, 0x00, 0x00, 0xFF, 0x7F, 0x60, 0x60, 0xE0,
    0xF0, 0x30, 0x1F, 0x0F, 0x00, 0x00, 0x00, 0xE1, 0xF1, 0x38, 0x18, 0x18,
    0x18, 0x18, 0x18, 0x18, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x7F, 0xFF, 0x81,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF,
    0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0xF0, 0xFF, 0xFF, 0x00, 0x00,
    0x00, 0x00, 0xFF, 0xFF, 0x80, 0x00, 0x00, 0x00, 0x00, 0xE1, 0xF1, 0x38,
    0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0xFF, 0xFF, 0x1C, 0x0F, 0x07, 0x07,
    0x02, 0x02, 0x02, 0x03, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1E,
    0x0F, 0x00, 0x00, 0x00, 0x01, 0x03, 0x07, 0x0C, 0x00, 0x00, 0x00, 0x00,
    0x07, 0x0F, 0x0E, 0x0C, 0x0C, 0x0C, 0x0C, 0x06, 0x07, 0x0F, 0x0F, 0x00,
    0x00, 0x00, 0x00, 0x03, 0x07, 0x0E, 0x0C, 0x0C, 0x0C, 0x0E, 0x07, 0x03,
    0x00, 0x00, 0x00, 0x00, 0x03, 0x0F, 0x0C, 0x0C, 0x0C, 0x0E, 0x07, 0x03,
    0x00, 0x0F, 0x0F, 0x00, 0x00, 0x00, 0x00, 0x0F, 0x0F, 0x0F, 0x00, 0x00,
    0x00, 0x00, 0x07, 0x0F, 0x0E, 0x0C, 0x0C, 0x0C, 0x0C, 0x06, 0x07, 0x0F, 0x0F
};

struct Image image_dracula = {
    .width = 88,
    .height = 5,
    .size = 445,
    .buffer = buffer_dracula
};

void ui_splash()
{
    display_invert(true);
    display_image(&image_dracula, 20, 2);
}
