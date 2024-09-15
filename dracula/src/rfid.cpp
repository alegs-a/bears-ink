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

MFRC522 mfrc522(0x2c, 3 /* Reset pin */); // Create MFRC522 instance.

bool rfid_init()
{
    return true;
}

void detect_new_card()
{
    if ( ! mfrc522.PICC_IsNewCardPresent()) {
        return;
    }

    // Read the new card's UID into mfrc522.uid
    if ( ! mfrc522.PICC_ReadCardSerial()) {
        return;
    }

    // I don't want to deal with the full NDEF spec, so I'll just read a
    // fixed address where the first record's URL happens to be.
    // It's split in two because we're only reading 16 bytes at a time.

    // Read the metadata and the first half of the URL
    byte data[18];
    byte data_len = 18;
    mfrc522.MIFARE_Read(6, data, &data_len);

    byte url_len = data[1]; // Payload length field

    const char *tld = "thebears.ink";
    if (strncmp(tld, reinterpret_cast<char*>(data + 4), 12) != 0) {
        printk("Unrecognised token:\n");
        mfrc522.PICC_DumpToSerial(&(mfrc522.uid));
        return;
    }

    // Read the second half of the magic URL
    data_len = 18;
    mfrc522.MIFARE_Read(10, data, &data_len);

    // Null-terminate the payload so it can be interpreted as a string.
    // 13 here represents "thebears.ink" and its leading 0x04 (https)
    data[url_len - 13] = '\0';

    enum TokenKind token_kind;
    const char *player1 = "/dracula/player1";
    const char *player2 = "/dracula/player2";
    const char *player3 = "/dracula/player3";
    const char *player4 = "/dracula/player4";
    const char *garlic = "/dracula/garlic";
    const char *sunlight = "/dracula/sun";
    const char *holy_water = "/dracula/water";
    if (url_len == 29 && strcmp(player1, reinterpret_cast<char*>(data)) == 0) {
        token_kind = Player1;
        printk("Found Bears Ink token: Player1\n");
    } else if (url_len == 29 && strcmp(player2, reinterpret_cast<char*>(data)) == 0) {
        token_kind = Player2;
        printk("Found Bears Ink token: Player2\n");
    } else if (url_len == 29 && strcmp(player3, reinterpret_cast<char*>(data)) == 0) {
        token_kind = Player3;
        printk("Found Bears Ink token: Player3\n");
    } else if (url_len == 29 && strcmp(player4, reinterpret_cast<char*>(data)) == 0) {
        token_kind = Player4;
        printk("Found Bears Ink token: Player4\n");
    } else if (url_len == 28 && strcmp(garlic, reinterpret_cast<char*>(data)) == 0) {
        token_kind = Garlic;
        printk("Found Bears Ink token: Garlic\n");
    } else if (url_len == 25 && strcmp(sunlight, reinterpret_cast<char*>(data)) == 0) {
        token_kind = Sunlight;
        printk("Found Bears Ink token: Sunlight\n");
    } else if (url_len == 27 && strcmp(holy_water, reinterpret_cast<char*>(data)) == 0) {
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
