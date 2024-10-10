#include "rfid.h"
#include "buzzer.h"
#include "room.h"
#include "MFRC522_I2C.h"

extern "C" {
#include "dracula.h"
}

#include <string.h>

#include <zephyr/device.h>
#include <zephyr/devicetree.h>
#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>

#include <zephyr/drivers/i2c.h>
#include <zephyr/logging/log.h>

LOG_MODULE_REGISTER(mfrc522, CONFIG_I2C_LOG_LEVEL);

void rfid_main(void *, void *, void *);

// Thread running the rfid driver.
static K_THREAD_DEFINE(rfid, RFID_THREAD_STACK_SIZE,
    rfid_main, NULL, NULL, NULL, RFID_THREAD_PRIORITY, 0, 0);

// Defined and initialised by the above macro.
extern const k_tid_t rfid_thread_id;

#define DT_DRV_COMPAT nxp_mfrc522
#define MFRC522_INIT_PRIO 64

// BUILD_ASSERT(MFRC522_INIT_PRIO > CONFIG_I2C_TCA954X_CHANNEL_INIT_PRIO,
//     "RFID readers must be initialised after their bus");

struct mfrc522_data {
};
struct mfrc522_cfg {
    struct i2c_dt_spec i2c;
    const char *room_name;
    enum RoomName room;
};

int mfrc522_init(const struct device *dev)
{
    const struct mfrc522_cfg *config = (const struct mfrc522_cfg *)dev->config;
    MFRC522 mfrc522(&config->i2c);
  
    if (!device_is_ready(config->i2c.bus)) {
        printk("I2C bus %s not ready\n", config->i2c.bus->name);
        return -ENODEV;
    }

    mfrc522.PCD_Init();
    byte v = mfrc522.PCD_ReadRegister(mfrc522.VersionReg);
    printk("MFRC522 @ %s Software Version: 0x%x\n", config->room_name, v);
    // When 0x00 or 0xFF is returned, communication probably failed
    if ((v == 0x00) || (v == 0xFF)) {
        printk("Room %s not ready\n", config->room_name);
        return -ENODEV;
    }

    mfrc522.PCD_SetAntennaGain(0x30);

    return 0;
}

#define MFRC522_INIT(inst)                                        \
    static mfrc522_data mfrc522_data_##inst = {                   \
    };                                                            \
    static const mfrc522_cfg mfrc522_cfg_##inst = {               \
        .i2c = I2C_DT_SPEC_INST_GET(inst),                        \
        .room_name = DT_PROP(DT_DRV_INST(inst), bearsink_room),   \
        .room = DT_STRING_TOKEN(DT_DRV_INST(inst), bearsink_room) \
    };                                                            \
    I2C_DEVICE_DT_INST_DEFINE(inst,                               \
           mfrc522_init, NULL,                                    \
           &mfrc522_data_##inst, &mfrc522_cfg_##inst,             \
           POST_KERNEL, MFRC522_INIT_PRIO, NULL);

#define MFRC522_ROOM_CASE(inst)                             \
    case DT_STRING_TOKEN(DT_DRV_INST(inst), bearsink_room): \
        return DEVICE_DT_INST_GET(inst);

#define MFRC522_DETECT_NEW(inst) \
    detect_new_card(mfrc522, (const struct mfrc522_cfg *)DEVICE_DT_INST_GET(inst)->config);


DT_INST_FOREACH_STATUS_OKAY(MFRC522_INIT)

const struct device *get_room(enum RoomName room)
{
    switch (room) {
        DT_INST_FOREACH_STATUS_OKAY(MFRC522_ROOM_CASE)
        default:
            return NULL;
    }
}

struct DraculaToken {
    enum RoomName room = NUM_ROOMS;
    unsigned char uid[7];
    enum TokenKind kind;
};

K_MUTEX_DEFINE(tokensMutex);

/**
 * @brief A list of the tokens currently on readers.
 *
 * This list is used primarily for internal tracking. When new tokens are
 * detected, they are appended to this list and a callback is fired to the game
 * logic. The game logic can also copy this list out with an rfid_ function.
 *
 * Empty entries are marked with a room name of NUM_ROOMS.
 *
 * This list is protected by tokensMutex
 */
DraculaToken currentTokens[MAX_TOKENS];

/**
 * @brief Scan the chosen room for new tokens
 *
 * This function will configure mfrc522 to use the given room internally.
 */
void detect_new_card(MFRC522 mfrc522, const struct mfrc522_cfg* room)
{
    mfrc522.i2c = &room->i2c;

    if (!mfrc522.PICC_IsNewCardPresent()) {
        return;
    }

    // Read the new card's UID into mfrc522.uid
    if (!mfrc522.PICC_ReadCardSerial()) {
        buzzer_send(READ_ERROR);
        return;
    }

    /*
     * Bears Ink tokens contain absolute URLs to https://thebears.ink/game/kind
     * as an identifier. They can be scanned e.g. a player's phone to take them
     * to our website, and created by players at will with a standard NFC
     * reader/writer app.
     *
     * Ideally, we should be parsing the NDEF metadata to extract the URL, but
     * I couldn't figure out how it fits in the tag, so I'm parsing a fixed
     * offset based on empirical evidence.
     */

    // Read the metadata and the first half of the URL
    byte data[18];
    byte data_len = 18;
    mfrc522.MIFARE_Read(6, data, &data_len);

    byte url_len = data[1]; // Payload length field

    // Make sure it's a Bears Ink token
    if (strncmp(reinterpret_cast<char*>(data + 4), "thebears.ink", 12) != 0) {
        buzzer_send(READ_ERROR);
        printk("Unrecognised token\n");
        return;
    }

    // Read the second half of the magic URL
    data_len = 18;
    mfrc522.MIFARE_Read(10, data, &data_len);

    // Null-terminate the payload so it can be interpreted as a string.
    // 13 here represents "thebears.ink" and the leading protocol byte
    data[url_len - 13] = '\0';
    char *game = reinterpret_cast<char*>(data);

    // Make sure it's a Dracula game token
    if (strncmp(game, "/dracula/", 9) != 0) {
        buzzer_send(READ_ERROR);
        printk("Unrecognised Bears Ink token: %s\n", game);
        mfrc522.PICC_DumpToSerial(&(mfrc522.uid));
        return;
    }

    // Check what kind of token it is
    char *kind = game + 9;
    struct Token this_token;
    this_token.room = room->room;
    if (strcmp(kind, "player1") == 0) {
        this_token.kind = Player1;
        printk("Found Bears Ink token in room %s: Player1\n", room->room_name);
    } else if (strcmp(kind, "player2") == 0) {
        this_token.kind = Player2;
        printk("Found Bears Ink token in room %s: Player2\n", room->room_name);
    } else if (strcmp(kind, "player3") == 0) {
        this_token.kind = Player3;
        printk("Found Bears Ink token in room %s: Player3\n", room->room_name);
    } else if (strcmp(kind, "player4") == 0) {
        this_token.kind = Player4;
        printk("Found Bears Ink token in room %s: Player4\n", room->room_name);
    } else if (strcmp(kind, "garlic") == 0) {
        this_token.kind = Garlic;
        printk("Found Bears Ink token in room %s: Garlic\n", room->room_name);
    } else if (strcmp(kind, "sun") == 0) {
        this_token.kind = Sunlight;
        printk("Found Bears Ink token in room %s: Sunlight\n", room->room_name);
    } else if (strcmp(kind, "water") == 0) {
        this_token.kind = HolyWater;
        printk("Found Bears Ink token in room %s: HolyWater\n", room->room_name);
    } else {
        buzzer_send(READ_ERROR);
        printk("Unrecognised Bears Ink token in room %s: (%d) %s\n", room->room_name, url_len, data);
        mfrc522.PICC_DumpToSerial(&(mfrc522.uid));
        return;
    }

    // Send the token to the game logic
    if (token_valid(this_token)) {
        buzzer_send(READ_OK);
        printk("Accepted\n");
    } else {
        buzzer_send(READ_ERROR);
        printk("Rejected\n");
    }

    // We've found a valid token; add it to currentTokens
    k_mutex_lock(&tokensMutex, K_FOREVER);
    for (int i = 0; i < MAX_TOKENS; i++) {
        if (currentTokens[i].room != NUM_ROOMS) {
            // This entry is occupied.
            continue;
        }

        currentTokens[i].room = this_token.room;
        currentTokens[i].kind = this_token.kind;
        memcpy(currentTokens[i].uid, mfrc522.uid.uidByte, 7);
        break;
    }
    k_mutex_unlock(&tokensMutex);

    // Halt the token. We can wake it up later when we want to check its presence.
    mfrc522.PICC_HaltA();
}

/**
 * @brief Check all the tokens in currentTokens to see if they're still there
 *
 * Takes tokensMutex internally and removes non-present tokens.
 */
void detect_current_cards(MFRC522 mfrc522)
{
    k_mutex_lock(&tokensMutex, K_FOREVER);
    for (int i = 0; i < MAX_TOKENS; i++) {
        if (currentTokens[i].room == NUM_ROOMS) {
            // Empty list entry; ignore.
            continue;
        }

        const struct mfrc522_cfg *room =
            (const struct mfrc522_cfg *)get_room(currentTokens[i].room)->config;
        mfrc522.i2c = &room->i2c;

        mfrc522.uid.size = 7;
        mfrc522.uid.sak = 0;
        memcpy(mfrc522.uid.uidByte, currentTokens[i].uid, 7);

        // Wake up all the tokens on this reader
        byte bufferATQA[2];
        byte bufferSize = sizeof(bufferATQA);
        byte result = mfrc522.PICC_WakeupA(bufferATQA, &bufferSize);

        // Select the token we're working with currently
        result = mfrc522.PICC_Select(&mfrc522.uid, 7 * 8);
        if (result == MFRC522::STATUS_TIMEOUT) {
            // This token probably disappeared; forget about it
            printk("Token %d removed from room %s.\n", currentTokens[i].kind, room->room_name);
            currentTokens[i].room = NUM_ROOMS;
        }

        mfrc522.PICC_HaltA();
    }
    k_mutex_unlock(&tokensMutex);
}

int rfid_get_tokens(struct Token *tokens)
{
    // Acquire the mutex so the rfid thread doesn't change the current tokens while we're reading
    // them out.
    k_mutex_lock(&tokensMutex, K_FOREVER);

    int tokenCount = 0;
    for (int i = 0; i < MAX_TOKENS; i++) {
        if (currentTokens[i].room == NUM_ROOMS) {
            // Empty list entry; ignore.
            continue;
        }

        tokens[tokenCount].kind = currentTokens[i].kind;
        tokens[tokenCount].room = currentTokens[i].room;
        tokenCount++;
    }

    k_mutex_unlock(&tokensMutex);

    return tokenCount;
}

void rfid_main(void *, void *, void *)
{
    MFRC522 mfrc522;

    for (;;) {
        DT_INST_FOREACH_STATUS_OKAY(MFRC522_DETECT_NEW)
        detect_current_cards(mfrc522);
    }
}
