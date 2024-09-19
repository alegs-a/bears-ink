#include "rfid.h"
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

MFRC522 mfrc522(0x2c, 3 /* Reset pin */);

bool rfid_init()
{
    // We can't do our initialisation here because the `mfrc522` variable above
    // isn't initialised by the C++ runtime until `rfid_main` is called.
    return true;
}

void detect_new_card()
{
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
        printk("Found Bears Ink token: Player1\n");
    } else if (strcmp(kind, "player2") == 0) {
        token_kind = Player2;
        printk("Found Bears Ink token: Player2\n");
    } else if (strcmp(kind, "player3") == 0) {
        token_kind = Player3;
        printk("Found Bears Ink token: Player3\n");
    } else if (strcmp(kind, "player4") == 0) {
        token_kind = Player4;
        printk("Found Bears Ink token: Player4\n");
    } else if (strcmp(kind, "garlic") == 0) {
        token_kind = Garlic;
        printk("Found Bears Ink token: Garlic\n");
    } else if (strcmp(kind, "sun") == 0) {
        token_kind = Sunlight;
        printk("Found Bears Ink token: Sunlight\n");
    } else if (strcmp(kind, "water") == 0) {
        token_kind = HolyWater;
        printk("Found Bears Ink token: HolyWater\n");
    } else {
        printk("Unrecognised Bears Ink token: (%d) %s\n", url_len, data);
        mfrc522.PICC_DumpToSerial(&(mfrc522.uid));
        return;
    }

    // We've found a valid token
    mfrc522.PICC_HaltA();
}

void rfid_main(void *, void *, void *)
{
    mfrc522.PCD_Init();
    byte v = mfrc522.PCD_ReadRegister(mfrc522.VersionReg);
    printk("MFRC522 Software Version: 0x%x\n", v);
    // When 0x00 or 0xFF is returned, communication probably failed
    if ((v == 0x00) || (v == 0xFF)) {
        printk("WARNING: Communication failure, is the MFRC522 properly connected?\n");
    }
    mfrc522.PCD_SetAntennaGain(0x50);
    byte gain = mfrc522.PCD_GetAntennaGain();
    printk("Antenna gain: 0x%x\n", gain);
    for (;;) {
        detect_new_card();
    }
}
