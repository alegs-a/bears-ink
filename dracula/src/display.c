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

#define COMMAND_DEFAULT_START_LINE 0x40
#define COMMAND_SET_MULTIPLEX_RATIO 0xA8
#define COMMAND_SET_SCAN_DIRECTION 0xC8
#define COMMAND_SET_SEGMENT_REMAP 0xA1
#define COMMAND_SET_DISPLAY_OFFSET 0xD3
#define COMMAND_SET_COM_HARDWARE_CONFIG 0xDA
#define COMMAND_SET_PRECHARGE_PERIOD 0xD9
#define COMMAND_SET_CLOCK_RATIO 0xD5
#define COMMAND_SET_VCOMH_DESELECT_LEVEL 0xDB
#define COMMAND_SET_CONTRAST 0x81
#define COMMAND_DISPLAY_ON 0xA4
#define COMMAND_SET_INVERSION_NORMAL 0xA6
#define COMMAND_SET_INVERSION_INVERTED 0xA7
#define COMMAND_SLEEP 0xAE
#define COMMAND_WAKE 0xAF
#define COMMAND_SET_IREF 0xAD
#define COMMAND_SET_CHARGE_PUMP 0x8D
#define COMMAND_NOP 0xE3
#define COMMAND_LOCK 0xFD
#define COMMAND_ADDRESS_MODE 0x20
#define COMMAND_SET_COLUMN_ADDRESS 0x21
#define COMMAND_SET_ROW_ADDRESS 0x22

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
    .operation = SPI_WORD_SET(8),
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

int display_send_command(uint8_t *data, int length);

int display_send_data(uint8_t *data, int length);

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

    // if (!spi_is_ready_dt(&spi)) {
    //     printk("display spi not ready\n");
    //     return false;
    // }

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

    // for (;;) {
    //     gpio_pin_set_dt(&display_pin_reset, 1);
    //     k_msleep(500);
    //     // gpio_pin_set_dt(&display_pin_reset, 0);
    //     // k_msleep(500);
    // }

    printk("reseting\n");
    display_reset();

    // unsigned char buffer[] = {
    //     COMMAND_SLEEP,
    //     COMMAND_ADDRESS_MODE, 0x0,
    //     COMMAND_DEFAULT_START_LINE,
    //     COMMAND_SET_SEGMENT_REMAP,
    //     COMMAND_SET_MULTIPLEX_RATIO, 63,
    //     COMMAND_SET_SCAN_DIRECTION,
    //     COMMAND_SET_DISPLAY_OFFSET, 0,
    //     COMMAND_SET_COM_HARDWARE_CONFIG, 0x12,
    //     COMMAND_SET_CLOCK_RATIO, 128,
    //     COMMAND_SET_PRECHARGE_PERIOD, 0xF1,
    //     COMMAND_SET_VCOMH_DESELECT_LEVEL, 0x30,
    //     COMMAND_SET_CONTRAST, 0xFF,
    //     COMMAND_DISPLAY_ON,
    //     COMMAND_SET_INVERSION_NORMAL,
    //     COMMAND_SET_IREF, 0x30,
    //     COMMAND_SET_CHARGE_PUMP, 0x14,
    //     COMMAND_WAKE
    // };

    unsigned char buffer[] = {0xAE, 0x20, 0x0, 0x40, 0xA1, 0xA8, 0x3F, 0xC8, 0xD3, 0x0, 0xDA, 0x12, 0xD5, 0x80, 0xD9, 0xF1, 0xDB, 0x30, 0x81, 0xFF, 0xA4, 0xA6, 0xAD, 0x30, 0x8D, 0x14, 0xAF};

    error = display_send_command(buffer, sizeof(buffer));
    if (error) {
        printk("display initialisation sequence failed with %i\n", error);
    }

    return error;
}

int display_send(uint8_t *data, int length)
{
    gpio_pin_set_dt(&display_pin_cs, 0);
    k_usleep(1);

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

int display_send_command(uint8_t *data, int length)
{
    printk("display send command ");
    for (int i = 0; i < length; i++) {
        printk("%x", data[i]);
    }
    printk("\n");

    // command low
    int error = gpio_pin_set_dt(&display_pin_dc, 0);
    if (error)
        return error;
    
    k_usleep(1);

    return display_send(data, length);
}

int display_send_data(uint8_t *data, int length)
{
    printk("display send data ");
    for (int i = 0; i < length; i++) {
        printk("%x", data[i]);
    }
    printk("\n");

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
    return display_send_command(buffer, sizeof(buffer));
}

void display_clear(unsigned char byte)
{
    uint8_t data[1] = {byte};

    gpio_pin_set_dt(&display_pin_cs, 0);
    k_usleep(1);

    struct spi_buf buffer = {
        .buf = data,
        .len = 1
    };

    struct spi_buf_set set = {
        .buffers = &buffer,
        .count = 1
    };

    for (int i = 0; i < (DISPLAY_ROW_MAX + 1) * (DISPLAY_COL_MAX + 1); i++) {
        int error = spi_write(display_device, spi, &set);
        if (error) {
            printk("spi failed to write: %i\n", error);
        }
    }
}

void display_sleep()
{
    uint8_t buffer[] = {COMMAND_SLEEP};
    display_send_command(buffer, sizeof(buffer));
}

void display_wake()
{
    uint8_t buffer[] = {COMMAND_WAKE};
    display_send_command(buffer, sizeof(buffer));
}

void display_set_contrast(uint8_t level)
{
    uint8_t buffer[] = {COMMAND_SET_CONTRAST, level};
    display_send_command(buffer, sizeof(buffer));
}

void display_invert(bool inverted)
{
    uint8_t buffer[1] = {inverted ? COMMAND_SET_INVERSION_INVERTED : COMMAND_SET_INVERSION_NORMAL};
    display_send_command(buffer, sizeof(buffer));
}

void display_address_mode(enum AddressMode mode)
{
    if (mode >= 3) {
        printk("invalid address mode %i\n", mode);
        return;
    }

    uint8_t buffer[2] = {COMMAND_ADDRESS_MODE, mode};
    display_send_command(buffer, sizeof(buffer));
}


int display_set_write_column_region(uint8_t column_begin, uint8_t column_end)
{
    if (column_begin > 127) {
        printk("cannot set row region start greater than 127\n");
        return EINVAL;
    }

    if (column_end > 127) {
        printk("cannot set row region end greater than 127\n");
        return EINVAL;
    }

    if (column_end < column_begin) {
        printk("cannot set row region start greater than row region end\n");
        return EINVAL;
    }

    uint8_t buffer[] = {COMMAND_SET_COLUMN_ADDRESS, column_begin, column_end};
    display_send_command(buffer, sizeof(buffer));

    return 0;
}

int display_set_write_row_region(uint8_t row_begin, uint8_t row_end)
{
    if (row_begin > 7) {
        printk("cannot set row region start greater than 7\n");
        return EINVAL;
    }

    if (row_end > 7) {
        printk("cannot set row region end greater than 7\n");
        return EINVAL;
    }

    if (row_end < row_begin) {
        printk("cannot set row region start greater than row region end\n");
        return EINVAL;
    }

    uint8_t buffer[] = {COMMAND_SET_ROW_ADDRESS, row_begin, row_end};
    display_send_command(buffer, sizeof(buffer));

    return 0;
}

int display_write(uint8_t row_begin, uint8_t row_end, uint8_t column_begin,
        uint8_t column_end, uint8_t *data, unsigned int n)
{
    int error = display_set_write_column_region(column_begin, column_end);
    if (error)
        return error;

    error = display_set_write_row_region(row_begin, row_end);
    if (error)
        return error;

    // k_msleep(1000);

    return display_send_data(data, n);
}
