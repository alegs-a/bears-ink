#include "display.h"

#include <zephyr/kernel.h>
#include <zephyr/sys/printk.h>
#include <zephyr/drivers/spi.h>
#include <zephyr/drivers/gpio.h>

// The pin configuration is the following:
// - Data In (Blue) -> PA_7 (pin A6) configured as SPI1_MOSI
// - Clock (Yellow) -> PA_1 (pin A1) configured as SPI1_SCK
// - Chip Select (Orange) -> PB_0 (pin D3) configured as SPI_NSS (logical not slave select)
// - Data Command (Green) -> PA_4 (pin A3) configured as GPIO
// - Reset (White) -> PA_0 (pin A0) configured as GPIO

// The stack size of the dracula thread.
#define DISPLAY_THREAD_STACK_SIZE 2048

// The thread priority of the dracula logic.
#define DISPLAY_THREAD_PRIORITY 5

enum Command {
    COMMAND_DEFAULT_START_LINE = 0x40,
    COMMAND_SET_MULTIPLEX_RATIO = 0xA8,
    COMMAND_SET_SCAN_DESCENDING = 0xC8,
    COMMAND_SET_SCAN_ASCENDING = 0xC0,
    COMMAND_SET_COM_NORMAL = 0xC0,
    COMMAND_SET_SEGMENT_REMAP_OFF = 0xA0,
    COMMAND_SET_SEGMENT_REMAP_ON = 0xA1,
    COMMAND_SET_DISPLAY_OFFSET = 0xD3,
    COMMAND_SET_COM_PIN_HARDWARE_CONFIG = 0xDA,
    COMMAND_SET_PRECHARGE_PERIOD = 0xD9,
    COMMAND_SET_CLOCK_RATIO = 0xD5,
    COMMAND_SET_VCOMH_DESELECT_LEVEL = 0xDB,
    COMMAND_SET_CONTRAST = 0x81,
    COMMAND_DISPLAY_ON = 0xA4,
    COMMAND_SET_INVERSION_NORMAL = 0xA6,
    COMMAND_SET_INVERSION_INVERTED = 0xA7,
    COMMAND_SLEEP = 0xAE,
    COMMAND_WAKE = 0xAF,
    COMMAND_SET_IREF = 0xAD,
    COMMAND_SET_CHARGE_PUMP = 0x8D,
    COMMAND_NOP = 0xE3,
    COMMAND_LOCK = 0xFD,
    COMMAND_ADDRESS_MODE = 0x20,
    COMMAND_SET_COLUMN_ADDRESS = 0x21,
    COMMAND_SET_ROW_ADDRESS = 0x22,
    COMMAND_DEACTIVATE_SCROLL = 0x2E
};

enum AddressMode {
    ADDRESS_MODE_HORISONTAL = 0b00,
    ADDRESS_MODE_VERTICAL = 0b01,
    ADDRESS_MODE_PAGE = 0b10
};

enum ComPinConfig {
    COM_PIN_SEQUENTIAL_NO_LR = 0b00000010,
    COM_PIN_ALTERNATIVE_NO_LR = 0b0001010,
    COM_PIN_SEQUENTIAL_LR = 0b00010010,
    COM_PIN_ALTERNATIVE_LR = 0b00011010
};

// Thread instance running the main lissajous thread.
// static K_THREAD_DEFINE(display, DISPLAY_THREAD_STACK_SIZE,
//     display_main, NULL, NULL, NULL, DISPLAY_THREAD_PRIORITY, 0, 0);

// Defined and initialised be the above macro.
extern const k_tid_t display_thread_id;

#define ZEPHYR_USER_NODE DT_PATH(zephyr_user)

// https://docs.zephyrproject.org/latest/build/dts/zephyr-user-node.html#gpios
// static const struct gpio_dt_spec reset_pin = GPIO_DT_SPEC_GET(DT_PATH(zephyr_user, display_reset), display_reset);
// static const struct gpio_dt_spec dc_pin = GPIO_DT_SPEC_GET(DT_PATH(zephyr_user, display_dc), display_dc);

static const struct device *display_device = DEVICE_DT_GET(DT_ALIAS(display));
static const struct gpio_dt_spec display_pin_dc = GPIO_DT_SPEC_GET(DT_NODELABEL(display_dc), gpios);
static const struct gpio_dt_spec display_pin_reset = GPIO_DT_SPEC_GET(DT_NODELABEL(display_reset), gpios);
static const struct gpio_dt_spec display_pin_cs = GPIO_DT_SPEC_GET(DT_NODELABEL(display_cs), gpios);

static const struct spi_config spi_configuration = {
    .frequency = 10000000,
    .operation = SPI_WORD_SET(8) | SPI_TRANSFER_MSB,
    .slave = 0,
    .cs = {
        .delay = 0,
        .gpio = {
            .port = NULL
        }
    }
};

static const struct spi_config *spi = &spi_configuration;

// Mutex protecting concurrent access to the display interface.
// static K_MUTEX_DEFINE(mutex);

// Queue of display requests being handled in sequence.
// K_FIFO_DEFINE(message_queue);

/**
 * @brief Set the column region to write to.
 * 
 * @pre `column_begin <= column_end`
 * 
 * @param column_begin The left column bound inclusive.
 * @param column_end The right column bound inclusive.
 * 
 * @returns Zero on success, EINVAL on bad arguments or another errno from SPI.
 */
int display_set_write_column_region(uint8_t column_begin, uint8_t column_end);

/**
 * @brief Set the row region to write to.
 * 
 * @pre `row_begin <= row_end`
 * 
 * @param row_begin The top row bound inclusive.
 * @param row_end The 
 * @returns Zero on success, EINVAL on bad arguments or another errno from SPI.
 */
int display_set_write_row_region(uint8_t row_begin, uint8_t row_end);

int display_send(uint8_t *data, int length);

int display_command(uint8_t *command, int length);

int display_data(uint8_t *data, int length);

/**
 * @brief Turn the display on.
 * @returns Zero on success or errno from SPI.
 */
int display_on();

int display_init()
{
    if (!device_is_ready(display_device)) {
        printk("display device not ready\n");
        return 1;
    }

    int error = gpio_pin_configure_dt(&display_pin_reset, GPIO_OUTPUT_ACTIVE);
    if (error) {
        printk("display reset pin configure failed\n");
        return error;
    }

    error = gpio_pin_configure_dt(&display_pin_dc, GPIO_OUTPUT_INACTIVE);
    if (error) {
        printk("display data/command pin configure failed\n");
        return error;
    }

    error = gpio_pin_configure_dt(&display_pin_cs, GPIO_OUTPUT_INACTIVE);
    if (error) {
        printk("display chip select pin configure failed\n");
        return error;
    }

    display_reset();

    unsigned char initialisation[] = {
        COMMAND_SLEEP,
        COMMAND_SET_CLOCK_RATIO, 128,
        COMMAND_SET_MULTIPLEX_RATIO, 63,
        COMMAND_SET_DISPLAY_OFFSET, 0,
        COMMAND_DEFAULT_START_LINE,
        COMMAND_SET_CHARGE_PUMP, 0x14,
        COMMAND_ADDRESS_MODE, ADDRESS_MODE_HORISONTAL,
        COMMAND_SET_SEGMENT_REMAP_ON,
        COMMAND_SET_SCAN_DESCENDING,
        COMMAND_SET_CONTRAST, 0xCF,
        COMMAND_SET_COM_NORMAL,
        COMMAND_SET_COM_PIN_HARDWARE_CONFIG, COM_PIN_SEQUENTIAL_LR,
        COMMAND_SET_PRECHARGE_PERIOD, 0xF1,
        COMMAND_SET_VCOMH_DESELECT_LEVEL, 0x40,
        COMMAND_DISPLAY_ON,
        COMMAND_SET_INVERSION_NORMAL,
        COMMAND_SET_IREF, 0x30,
        COMMAND_DEACTIVATE_SCROLL,
        COMMAND_WAKE
    };

    error = display_command(initialisation, sizeof(initialisation));
    if (error) {
        printk("display initialisation sequence failed with %i\n", error);
    }

    k_usleep(1);

    return error;
}

int display_send(uint8_t *data, int length)
{
    struct spi_buf buffer = {
        .buf = data,
        .len = length
    };

    struct spi_buf_set set = {
        .buffers = &buffer,
        .count = 1
    };

    int error = spi_write(display_device, spi, &set);
    if (error) {
        printk("spi failed to write: %i\n", error);
    }

    return error;
}

int display_command(uint8_t *data, int length)
{
    printk("display send command ");
    for (int i = 0; i < length; i++) {
        printk("%02x ", data[i]);
    }
    printk("\n");

    // command low
    int error = gpio_pin_set_dt(&display_pin_dc, 0);
    if (error)
        return error;

    k_usleep(1);

    return display_send(data, length);
}

int display_data(uint8_t *data, int length)
{
    // printk("display send data ");
    // for (int i = 0; i < length; i++) {
    //     printk("%02x", data[i]);
    // }
    // printk("\n");

    // command high
    int error = gpio_pin_set_dt(&display_pin_dc, 1);
    if (error)
        return error;

    k_usleep(1);

    return display_send(data, length);
}

int display_reset()
{
    gpio_pin_set_dt(&display_pin_reset, 0);
    k_usleep(4);
    gpio_pin_set_dt(&display_pin_reset, 1);
    k_msleep(150);

    return 0;
}

int display_on()
{
    uint8_t buffer[] = {COMMAND_DISPLAY_ON};
    return display_command(buffer, sizeof(buffer));
}

void display_clear(unsigned char byte)
{
    display_set_bounds(0, 0xFF, 0, 8);

    for (int i = 0; i < 1024; i++) {
        display_data(&byte, 1);
    }

    byte = 0x00;
    for (int i = 0; i < 3; i++)
        display_data(&byte, 1);
}

void display_sleep()
{
    uint8_t buffer[] = {COMMAND_SLEEP};
    display_command(buffer, sizeof(buffer));
}

void display_wake()
{
    uint8_t buffer[] = {COMMAND_WAKE};
    display_command(buffer, sizeof(buffer));
}

void display_set_contrast(uint8_t level)
{
    uint8_t buffer[] = {COMMAND_SET_CONTRAST, level};
    display_command(buffer, sizeof(buffer));
}

void display_invert(bool inverted)
{
    uint8_t buffer[1] = {inverted ? COMMAND_SET_INVERSION_INVERTED : COMMAND_SET_INVERSION_NORMAL};
    display_command(buffer, sizeof(buffer));
}

int display_set_bounds(uint8_t column_begin, uint8_t column_end, uint8_t row_begin,
        uint8_t row_end)
{
    uint8_t buffer[] = {
        COMMAND_SET_ROW_ADDRESS, row_begin, row_end,
        COMMAND_SET_COLUMN_ADDRESS, column_begin, column_end
    };

    display_command(buffer, sizeof(buffer));

    return 0;
}

int display_write(uint8_t column_begin, uint8_t column_end, uint8_t row_begin,
        uint8_t row_end, uint8_t *data, unsigned int n)
{
    display_set_bounds(column_begin, column_end, row_begin, row_end);
    display_data(data, n);
    return 1;
}
