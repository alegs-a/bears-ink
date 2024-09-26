#include "rfid.h"
#include "room.h"
#include "MFRC522_I2C.h"

#include <string.h>

#include <zephyr/device.h>
#include <zephyr/devicetree.h>
#include <zephyr/kernel.h>
#include <zephyr/sys/mutex.h>
#include <zephyr/sys/printk.h>

#include <zephyr/drivers/i2c.h>

// Thread running the rfid driver.
static K_THREAD_DEFINE(rfid, RFID_THREAD_STACK_SIZE,
    rfid_main, NULL, NULL, NULL, RFID_THREAD_PRIORITY, 0, 0);

// Defined and initialised by the above macro.
extern const k_tid_t rfid_thread_id;

MFRC522 mfrc522(0x2c);

struct DraculaToken {
    enum RoomName room = MAXIMUM_ROOM;
    unsigned char uid[7];
    enum TokenKind kind;
};

#define MAX_TOKENS 64

K_MUTEX_DEFINE(tokensMutex);

/**
 * @brief A list of the tokens currently on readers.
 *
 * This list is used primarily for internal tracking. When new tokens are
 * detected, they are appended to this list and a callback is fired to the game
 * logic. The game logic can also copy this list out with an rfid_ function.
 *
 * Empty entries are marked with a room name of MAXIMUM_ROOM.
 *
 * This list is protected by tokensMutex
 */
DraculaToken currentTokens[MAX_TOKENS];

/**
 * @brief Set up the I2C bus and MFRC522 library to communicate with the chosen room.
 *
 * This will modify the state of the TCA9548A bus multiplexer, and the _chipAddress
 * variable in mfrc522.
 */
void select_room(enum RoomName room)
{
    mfrc522._chipAddress = 0x2c + room % 4;
}

/**
 * @brief Scan the chosen room for new tokens
 *
 * This function will call select_room to ready the bus internally. If a new token
 * is found, tokensMutex will be taken and the token will be added to currentTokens.
 */
void detect_new_card(enum RoomName room)
{
    select_room(room);

    if (!mfrc522.PICC_IsNewCardPresent()) {
        return;
    }

    // Read the new card's UID into mfrc522.uid
    if (!mfrc522.PICC_ReadCardSerial()) {
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
        printk("Unrecognised token:\n");
        mfrc522.PICC_DumpToSerial(&(mfrc522.uid));
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
        printk("Unrecognised Bears Ink token: %s\n", game);
        mfrc522.PICC_DumpToSerial(&(mfrc522.uid));
        return;
    }

    // Check what kind of token it is
    char *kind = game + 9;
    enum TokenKind token_kind;
    if (strcmp(kind, "player1") == 0) {
        token_kind = Player1;
        printk("Found Bears Ink token in room %d: Player1\n", room);
    } else if (strcmp(kind, "player2") == 0) {
        token_kind = Player2;
        printk("Found Bears Ink token in room %d: Player2\n", room);
    } else if (strcmp(kind, "player3") == 0) {
        token_kind = Player3;
        printk("Found Bears Ink token in room %d: Player3\n", room);
    } else if (strcmp(kind, "player4") == 0) {
        token_kind = Player4;
        printk("Found Bears Ink token in room %d: Player4\n", room);
    } else if (strcmp(kind, "garlic") == 0) {
        token_kind = Garlic;
        printk("Found Bears Ink token in room %d: Garlic\n", room);
    } else if (strcmp(kind, "sun") == 0) {
        token_kind = Sunlight;
        printk("Found Bears Ink token in room %d: Sunlight\n", room);
    } else if (strcmp(kind, "water") == 0) {
        token_kind = HolyWater;
        printk("Found Bears Ink token in room %d: HolyWater\n", room);
    } else {
        printk("Unrecognised Bears Ink token in room %d: (%d) %s\n", mfrc522._chipAddress, url_len, data);
        mfrc522.PICC_DumpToSerial(&(mfrc522.uid));
        return;
    }

    // We've found a valid token; add it to currentTokens
    k_mutex_lock(&tokensMutex, K_FOREVER);
    for (int i = 0; i < MAX_TOKENS; i++) {
        if (currentTokens[i].room != MAXIMUM_ROOM) {
            // This entry is occupied.
            continue;
        }

        currentTokens[i].room = room;
        currentTokens[i].kind = token_kind;
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
void detect_current_cards()
{
    k_mutex_lock(&tokensMutex, K_FOREVER);
    for (int i = 0; i < MAX_TOKENS; i++) {
        if (currentTokens[i].room == MAXIMUM_ROOM) {
            // Empty list entry; ignore.
            continue;
        }

        mfrc522.uid.size = 7;
        mfrc522.uid.sak = 0;
        memcpy(mfrc522.uid.uidByte, currentTokens[i].uid, 7);

        select_room(currentTokens[i].room);

        // Wake up all the tokens on this reader
        byte bufferATQA[2];
        byte bufferSize = sizeof(bufferATQA);
        byte result = mfrc522.PICC_WakeupA(bufferATQA, &bufferSize);

        // Select the token we're working with currently
        result = mfrc522.PICC_Select(&mfrc522.uid, 7 * 8);
        if (result == MFRC522::STATUS_TIMEOUT) {
            // This token probably disappeared; forget about it
            printk("Token %d removed from room %d.\n", currentTokens[i].kind, currentTokens[i].room);
            currentTokens[i].room = MAXIMUM_ROOM;
        }

        mfrc522.PICC_HaltA();
    }
    k_mutex_unlock(&tokensMutex);
}

void rfid_main(void *, void *, void *)
{
    mfrc522._chipAddress = 0x2c;
    mfrc522.PCD_Init();
    byte v = mfrc522.PCD_ReadRegister(mfrc522.VersionReg);
    printk("MFRC522 @ %x Software Version: 0x%x\n", mfrc522._chipAddress, v);
    // When 0x00 or 0xFF is returned, communication probably failed
    if ((v == 0x00) || (v == 0xFF)) {
        printk("WARNING: Communication failure, is the MFRC522 properly connected?\n");
    }
    mfrc522.PCD_SetAntennaGain(0x50);

    mfrc522._chipAddress = 0x2e;
    mfrc522.PCD_Init();
    v = mfrc522.PCD_ReadRegister(mfrc522.VersionReg);
    printk("MFRC522 @ %x Software Version: 0x%x\n", mfrc522._chipAddress, v);
    // When 0x00 or 0xFF is returned, communication probably failed
    if ((v == 0x00) || (v == 0xFF)) {
        printk("WARNING: Communication failure, is the MFRC522 properly connected?\n");
    }
    mfrc522.PCD_SetAntennaGain(0x50);

    for (;;) {
        detect_new_card(NHALL);
        detect_new_card(GUARDEDWAY);

        detect_current_cards();
    }
}
