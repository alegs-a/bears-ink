#include "display.h"

#include <zephyr/irq.h>
#include <zephyr/device.h>
#include <zephyr/kernel.h>
#include <zephyr/sys/printk.h>
#include <zephyr/drivers/gpio.h>
#include <zephyr/drivers/i2c.h>

struct ssd1309_data {
};
struct ssd1309_config {
    struct i2c_dt_spec i2c;
};

/**
 * @brief Commands sent to the display driver.
 */
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
    COMMAND_SET_CLOCK_RATIO_AND_FREQ = 0xD5,
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

/**
 * @brief Addressing modes for writing to the display.
 */
enum AddressMode {
    ADDRESS_MODE_HORISONTAL = 0b00,
    ADDRESS_MODE_VERTICAL = 0b01,
    ADDRESS_MODE_PAGE = 0b10
};

/**
 * @brief Physical pixel pin driver configurations.
 */
enum ComPinConfig {
    COM_PIN_SEQUENTIAL_NO_LR = 0b00000010,
    COM_PIN_ALTERNATIVE_NO_LR = 0b0001010,
    COM_PIN_SEQUENTIAL_LR = 0b00010010,
    COM_PIN_ALTERNATIVE_LR = 0b00011010
};

/**
 * @brief The routine running for the display thread.
 */
void display_main(void*, void*, void*);

// Thread instance running the main lissajous thread.
// static K_THREAD_DEFINE(display, DISPLAY_THREAD_STACK_SIZE,
//     display_main, NULL, NULL, NULL, DISPLAY_THREAD_PRIORITY, 0, 0);

// Defined and initialised be the above macro.
extern const k_tid_t display_thread_id;

/* All following bytes will contain commands */
#define SSD1309_CONTROL_ALL_BYTES_CMD		0x00
/* All following bytes will contain data */
#define SSD1309_CONTROL_ALL_BYTES_DATA		0x40

// https://docs.zephyrproject.org/latest/build/dts/zephyr-user-node.html#gpios
static const struct gpio_dt_spec display_pin_reset = GPIO_DT_SPEC_GET(DT_NODELABEL(display_reset), gpios);

// Mutex protecting concurrent access to the display interface.
// static K_MUTEX_DEFINE(mutex);

// Queue of display requests being handled in sequence.
K_FIFO_DEFINE(display_queue);

/**
 * @brief Send commands to the display.
 * 
 * @param command The command buffer to send.
 * @param length The length of the command buffer in bytes.
 * 
 * @returns Zero on success or a non-zero error number on failure.
 */
int display_command(const struct device *dev, const uint8_t *command, int length)
{
    const struct ssd1309_config *config = dev->config;
    return i2c_burst_write_dt(&config->i2c, SSD1309_CONTROL_ALL_BYTES_CMD, command, length);
}

/**
 * @brief Send data to the display.
 * 
 * @param data The data buffer to send.
 * @param length The length of the data buffer in bytes.
 * 
 * @returns Zero on success or a non-zero error number on failure.
 */
int display_data(const struct device *dev, const uint8_t *data, int length)
{
    const struct ssd1309_config *config = dev->config;
    return i2c_burst_write_dt(&config->i2c, SSD1309_CONTROL_ALL_BYTES_DATA, data, length);
}

/**
 * @brief Reset the display.
 * @returns Zero on success or a non-zero error number on failure.
 */
int display_reset()
{
    gpio_pin_set_dt(&display_pin_reset, 0);
    k_usleep(4);
    gpio_pin_set_dt(&display_pin_reset, 1);
    k_msleep(150);

    return 0;
}

int ssd1309_init(const struct device *dev)
{
    const struct ssd1309_config *config = dev->config;
    if (!device_is_ready(config->i2c.bus)) {
        printk("I2C bus %s not ready\n", config->i2c.bus->name);
        return -ENODEV;
    }

    // Configure the reset pin to be initially high (reset is low).
    int error = gpio_pin_configure_dt(&display_pin_reset, GPIO_OUTPUT_ACTIVE);
    if (error) {
        printk("display reset pin configure failed\n");
        return error;
    }
    // Hard reset before initialisation.
    display_reset();

    unsigned char initialisation[] = {
        COMMAND_SLEEP,
        COMMAND_SET_CLOCK_RATIO_AND_FREQ, 128,
        COMMAND_SET_MULTIPLEX_RATIO, 63,
        COMMAND_SET_PRECHARGE_PERIOD, 0x22,
        COMMAND_SET_CHARGE_PUMP, 20,
        COMMAND_SET_VCOMH_DESELECT_LEVEL, 0x34,
        COMMAND_SET_IREF, 0x30,
        COMMAND_SET_COM_NORMAL,
        COMMAND_SET_COM_PIN_HARDWARE_CONFIG, COM_PIN_SEQUENTIAL_LR,
        COMMAND_SET_DISPLAY_OFFSET, 0,
        COMMAND_SET_SEGMENT_REMAP_ON,
        COMMAND_SET_SCAN_DESCENDING,
        COMMAND_DEFAULT_START_LINE,
        COMMAND_ADDRESS_MODE, ADDRESS_MODE_HORISONTAL,
        COMMAND_SET_CONTRAST, 255,
        COMMAND_DISPLAY_ON,
        COMMAND_SET_INVERSION_NORMAL,
        COMMAND_DEACTIVATE_SCROLL,
        COMMAND_WAKE
    };

    error = display_command(dev, initialisation, sizeof(initialisation));
    if (error) {
        printk("display initialisation sequence failed with %i\n", error);
        return -ENODEV;
    }

    k_usleep(1);

    display_clear(dev, 0x00);

    display_invert(dev, true);

    return error;
}

/**
 * @brief Set the area to which data should be displayed.
 * 
 * The display has 8 rows. Each row has 128 columns. Each row is 8 pixels tall.
 * Each of the 8 bits in a byte written to the display sets the 8 pixel states
 * from LSB to MSH at a given row and column.
 * 
 * @param column_begin The starting column from 0 to 127 inclusive.
 * @param column_end The end column from 0 to 127 inclusive.
 * @param row_begin The starting row from 0 to 7 inclusive.
 * @param row_end The end row from 0 to 7 inclusive.
 * 
 * @returns If the display bounds were set.
 */
int display_set_bounds(const struct device *dev, uint8_t column_begin, uint8_t column_end,
        uint8_t row_begin, uint8_t row_end)
{
    uint8_t buffer[] = {
        COMMAND_SET_ROW_ADDRESS, row_begin, row_end,
        COMMAND_SET_COLUMN_ADDRESS, column_begin, column_end
    };

    return display_command(dev, buffer, sizeof(buffer));
}

void display_set_contrast(const struct device *dev, uint8_t level)
{
    uint8_t buffer[] = {COMMAND_SET_CONTRAST, level};
    display_command(dev, buffer, sizeof(buffer));
}

void display_invert(const struct device *dev, bool inverted)
{
    uint8_t buffer[1] = {inverted ? COMMAND_SET_INVERSION_INVERTED : COMMAND_SET_INVERSION_NORMAL};
    display_command(dev, buffer, sizeof(buffer));
}

#define DT_DRV_COMPAT solomon_ssd1309
#define SSD1309_INIT_PRIO 70
#define SSD1309_INIT(inst)                                        \
    static struct ssd1309_data ssd1309_data_##inst = {            \
    };                                                            \
    static const struct ssd1309_config ssd1309_cfg_##inst = {     \
        .i2c = I2C_DT_SPEC_INST_GET(inst),                        \
    };                                                            \
    I2C_DEVICE_DT_INST_DEFINE(inst,                               \
           ssd1309_init, NULL,                                    \
           &ssd1309_data_##inst, &ssd1309_cfg_##inst,             \
           POST_KERNEL, SSD1309_INIT_PRIO, NULL);

DT_INST_FOREACH_STATUS_OKAY(SSD1309_INIT)


// The device structure.
static const struct device *display_device = DEVICE_DT_GET(DT_NODELABEL(display));

/**
 * @brief An image requst to display
 */
struct ImageMessage {

    /// Pointer to the image to display.
    struct Image *image;

    /// The x coordinate of the image.
    unsigned int x;

    /// The y coordinate of the image.
    unsigned int y;
};

const uint8_t zeroes[1024];

int display_write(uint8_t column_begin, uint8_t column_end, uint8_t row_begin,
        uint8_t row_end, const uint8_t *data, unsigned int n)
{
    int error = display_set_bounds(display_device, column_begin, column_end, row_begin, row_end);
    if (error)
        return error;

    error = display_data(display_device, data, n);
    if (error)
        return error;

    return 0;
}

void display_clear()
{
    display_write(0, DISPLAY_WIDTH - 1, 0, DISPLAY_HEIGHT - 1,
                  zeroes, 1024);
}

int display_image(struct Image *image, unsigned int x, unsigned int y)
{
    // Allocate a message to be passed into the read queue.
    struct ImageMessage *message =
            (struct ImageMessage*)k_malloc(sizeof(struct ImageMessage));

    message->image = image;
    message->x = x;
    message->y = y;

    int error = display_write(
        message->x,
        message->x + message->image->width - 1,
        message->y,
        message->y + message->image->height - 1,
        message->image->buffer,
        message->image->size
    );

    if (error) {
        printk("failed to display image\n");
    }

    k_free(message);
    return 0;
}

void display_main(void*, void*, void*)
{
    for (;;) {
        // Get the next image display request, waiting forever.
        struct ImageMessage *message = k_fifo_get(&display_queue, K_FOREVER);

        int error = display_write(
            message->x,
            message->x + message->image->width - 1,
            message->y,
            message->y + message->image->height - 1,
            message->image->buffer,
            message->image->size
        );

        if (error) {
            printk("failed to display image\n");
        }

        k_free(message);
    }
}
