#include "rfid.h"
#include "MFRC522_I2C.h"

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

MFRC522 mfrc522(0x2c, 3 /* Reset pin */);	// Create MFRC522 instance.

bool rfid_init()
{
    return true;
}

void detect_new_card()
{
	  if ( ! mfrc522.PICC_IsNewCardPresent()) {
    		return;
	  }
	  if ( ! mfrc522.PICC_ReadCardSerial()) {
		    return;
	  }
	  mfrc522.PICC_DumpToSerial(&(mfrc522.uid));
	  printk("Tag detected!");
    
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
	  mfrc522.PCD_SetAntennaGain(0x70);
	  byte gain = mfrc522.PCD_GetAntennaGain();
	  printk("Antenna gain: 0x%x\n", gain);
    for (;;) {
        detect_new_card();
    }
}
