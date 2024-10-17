

Table of Contents:
1. [Introduction](#1-introduction)
2. [Rules](#2-rules)
    1. [Setup](#21-setup)
    2. [Gameplay](#22-gameplay)
    3. [Items](#23-items)
    4. [Dracula's Turn](#24-draculas-turn)
    5. [Game End](#25-game-end)
3. [Development Environment](#3-development-environment)
4. [Building & Flashing](#4-building--flashing)
5. [Hardware & BOM](#5-hardware--bom)
6. [Connecting the Board](#6-connecting-the-board)
7. [Project Overview](#7-project-overview)
8. [Repository Structure](#8-repository-structure)
9. [Using the Display](#9-using-the-display)
10. [Privacy and Ethical considerations](#10-privacy-and-ethical-considerations)
11. [Issues](#11-issues)
12. [Additional Resources](#11-additional-resources)

## 1. Introduction

Welcome to the core repository of dracula, an interactive and collaborative
technology enhanced board game of hidden information. In Dracula, players
race to track down and defeat Dracula before they are all bitten. On each
turn, a player can move to a new room on a castle board, cast light to block
Dracula from moving, throw garlic to detect him, and throw holy water to attack
him. Each board game piece is RFID tagged to be read by RFID readers in the
board. After all players turns, the game's computer uses the RFID information to
automate Dracula's actions and display game information to the players using an
LCD screen and LEDs, to provide an engaging user experience. Will the players
be able to take down Dracula in time?

The majority of our original work can be found in the `dracula/src` directory
(see [Repository Structure](#7-repository-structure)).

## 2. Rules

### 2.1 Setup

The setup is as follows:
1. The four player meeples are placed in the corner rooms of the board.
2. The Dracula meeple is placed in the dungeon.
3. Each player receives one Sunshine token and one Holy Water token, as seen in the table below.

The players start with 5 lives shared among them, and Dracula has 3.

### 2.2 Gameplay

Beginning with an arbitrary player, on each players turn they may choose to take any of the following actions, in any order:

1. Move to an adjacent room (at most once).
2. Throw a garlic into a room (at most 4 times, or the number of garlics remaining that round).
3. Cast Sunshine in a Bottle, ending the players turn.
4. Throwing holy water into a room, ending the players turn.

A player may also choose to take *no* actions and receive an extra item (Sunshine or Holy Water) of their choosing. A player's turn is over when they press the confirm turn button.

### 2.3 Items

Items can be used in the player's current room, or any adjacent room .To use an
item, place it on the reader in the desired room. The item may be left there
until the end of the round, or moved to make space for a new token.

| Item                 | Finite                       | Description                                                                                                                                                                                                                                                                            |
| -------------------- | ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Garlic               | Four per round (any players) | When placed on a room the player is in or adjacent to, checks if dracula is in the room.                                                                                                                                                                                                                      |
| Sunshine in a Bottle | Yes                          | Illuminates a room with bright sunlight until the end of Dracula's turn. If a room becomes illuminated Dracula must immediately leave the room, and cannot enter again until after his turn. He must leave through a doorway other than the one that the sunshine was thrown through. |
| Holy water           | Yes                          | If holy water is thrown into a room with Dracula in it, he takes 1 point of damage.                                                                                                                                                                                        |

### 2.4 Dracula's Turn

Dracula takes his turn after all four player turns have finished. He can move up
to *three* rooms in one turn. If - at any point during his turn - he is in the
same room as a player he may bite that player, causing the player team to lose a
life and the bitten player to lose their next turn. Fortunately, Dracula doesn't
like spoiled food, so he'll wait at least one turn to bite a player for a second
time. He may bite as many players as he likes in a single turn.

If Dracula was hit by holy water during a player's turn he may still move, but
cannot bite until the end of the round.

### 2.5 Game End

The player team wins if Dracula runs out of lives. Dracula wins if he bites the
players a total of five times.

## 3. Development Environment

Only windows and linux are supported (not apple architecture).

Firstly, download the required programs for the development environment:
- [VSCode](https://code.visualstudio.com/download) - Our selected integrated
  development environment.
- [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) - Required for
  running docker on windows machines.
- [Docker](https://www.docker.com/) - Used to support the development
  environment across multiple platforms, and ensures the toolchain is not
  required locally.
- [usbip](https://usbip.sourceforge.net/) - Used to forward usb connections to
  docker in windows wsl development environments.
- VSCode Extensions. Most critically are:
  - [`C/C++`](https://marketplace.visualstudio.com/items?itemName=ms-vscode.cpptools)
    for C syntax highlighting and intellisense.
  - [`Dev Containers`](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
    for supporting docker dev-containers.
  - More are recommended when opening the repository in VSCode.

The microcontroller used in this project is an
[STM32L432KC](https://www.st.com/en/microcontrollers-microprocessors/stm32l432kc.html)
on the [Nucleo-L432KC](https://www.st.com/en/evaluation-tools/nucleo-l432kc.html)
development board. Specifics about these boards are not required, other than the
pinout configuration on the development board for connecting wires together.

To get started, use a micro USB to USB cable to plug the development board into
your computer. If you do not have a USB type A port on your computer, you must
use a dongle adaptor. The device should flash red to indicate it is powered, but
the USB communication chip is not receiving any data.

> [!NOTE]
> Windows users will need to forward USB connections to wsl.
> - Open a command terminal by search for "Command Prompt" or your favourite
>   terminal program.
> - Run `usbipd list` to list the current USB devices.
> - This should reveal a device with a name prefixed with "ST-Link". Note down
>   the matching `BUSID`, that are two numbers separated by a dash.
> - Run `usbipd attach -b <bus_id> --wsl`. For example, for a bus id of "4-1"
>   you would run `usbipd attach -b 4-1 --wsl`. This may require adminstrator
>   permissions. Ensure the bus id is typed correctly, matches the ST-Link
>   device, and the device is plugged in.
> - The usb device should now be forwarded to docker.

Open the repository folder in VSCode, then reopen the folder in the docker 
development container. To do so either click the pop-up
`"Reopen folder in container"` on the bottom right or press `F1`, type
`"Dev Containers: Reopen in Container"` and press enter. This will build the
development container using the VSCode devcontaner specification at
[/.devcontainer/devcontainer.json](.devcontainer/devcontainer.json) and the
corresponding [/.devcontainer/dockerfile](/.devcontainer/dockerfile), and may
take up to 30 minutes to complete depending on download speed. Once completed,
the folder will reopen in the development environment.

## 4. Building & Flashing

This project is built using the
[Zephyr](https://docs.zephyrproject.org/latest/index.html) real time operating
system and development framework. Not only does Zephyr provide an abstraction
layer for interfacing with the microcontroller, it also provides building and
flashing utilities. In this project, building, flashing and other repetitive
tasks are automated using [VSCode
tasks](https://code.visualstudio.com/Docs/editor/tasks). All the tasks are
defined in the [/.vscode/tasks.json](/.vscode/tasks.json) and require the
project to be opened the devcontainer. Note that these tasks call scripts under
[`/scripts`](scripts) that can be run from a command line to support developers
that have a local Zephyr installation and would rather use another environment
such as vim. To run a task, press `F1`, type `"Tasks: Run Task"` and select one
from the drop down list.

- To build the project, run the `"Build"` task.
- To clean the build, run the `"Clean"` task.
- To flash a built project to the microcontroller, run the `"Flash"` task.

## 5. Hardware & BOM

The full bill of materials for the electronics is as follows:

| Component                                | Link                                                                                                                          | #   | Individual Cost                                                                                                                                         | Total Cost |
| ---------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- | --- | ------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------- |
| Microcontroller                          | [Nucleo-L432KC](https://www.st.com/en/evaluation-tools/nucleo-l432kc.html)                                                    | 1   | [16.27$ (Digikey)](https://www.digikey.com.au/en/products/detail/stmicroelectronics/NUCLEO-L432KC/6132763?s=N4IgTCBcDaIHYFcDGAbApgewAQoCwGYwBrJEAXQF8g) | 16.27$     |
| Display                                  | [Waveshare 2.42inch OLED Module](https://core-electronics.com.au/242inch-oled-display-module-128x64px.html)                   | 1   | [22.60$ (Core Electronics)](https://core-electronics.com.au/242inch-oled-display-module-128x64px.html)                                                  | 22.60$     |
| LED Strip                                | [W2812 RGB LED Strip 2m](https://core-electronics.com.au/digital-rgb-led-strip-120-led-black.html)                            | 1   | [19.60$ (Core Electronics)](https://core-electronics.com.au/digital-rgb-led-strip-120-led-black.html)                                                   | 19.60$     |
| RFID Tags                                | [NTAG213 25mm Coin RFID Tag](https://core-electronics.com.au/ntag213-coin-25mm-white.html)                                    | 16  | [1.49$ (Core Electronics)](https://core-electronics.com.au/ntag213-coin-25mm-white.html)                                                                | 23.84$     |
| RFID Sensor                              | [PiicoDev RFID Module](https://core-electronics.com.au/piicodev-rfid-module.html)                                             | 22  | [8.59$ (Core Electronics) Bulk](https://core-electronics.com.au/piicodev-rfid-module.html)                                                              | 188.98$    |
| RFID I2C Bus 200mm                       | [PiicoDev Cable 200mm](https://core-electronics.com.au/piicodev-cable-200mm.html)                                             | 14  | [1.06$ (Core Electronics)](https://core-electronics.com.au/piicodev-cable-200mm.html)                                                                   | 14.84$     |
| RFID I2C Bus 500mm                       | [PiicoDev Cable 500mm](https://core-electronics.com.au/piicodev-cable-500mm.html)                                             | 6   | [1.4$ (Core Electronics)](https://core-electronics.com.au/piicodev-cable-500mm.html)                                                                    | 8.40$      |
| RFID Sensor I2C Breakout                 | [PiicoDev Adaptor for Breadboards](https://core-electronics.com.au/piicodev-breadboard-adapter.html)                          | 6   | [2.61$ (Core Electronics)](https://core-electronics.com.au/piicodev-breadboard-adapter.html)                                                            | 15.66$     |
| RFID I2C Multiplexer                     | [TCA9548A I2C Multiplexer](https://core-electronics.com.au/tca9548a-i2c-multiplexer.html)                                     | 1   | [13.05$ (Core Electronics)](https://core-electronics.com.au/tca9548a-i2c-multiplexer.html)                                                              | 13.05$     |
| Piezo Buzzer                             | [Piezo Buzzer](https://core-electronics.com.au/piezo-buzzer.html)                                                             | 1   | [0.46$ (Core Electronics)](https://core-electronics.com.au/piezo-buzzer.html)                                                                           | 0.46$      |
| Battery                                  | [LiPo Battery 2000mAh](https://core-electronics.com.au/polymer-lithium-ion-battery-2000mah-38459.html)                        | 1   | [17.55$ (Core Electronics)](https://core-electronics.com.au/polymer-lithium-ion-battery-2000mah-38459.html)                                             | 17.55$     |
| Battery Management 5V Supply             | [SparkFun LiPo Charger/Booster - 5V/1A](https://www.sparkfun.com/products/14411)                                              | 1   | [28.35$ (Core Electronics)](https://core-electronics.com.au/sparkfun-lipo-charger-booster-5v-1a.html)                                                   | 28.35$     |
| 3.3V Stepdown                            | [SparkFun BabyBuck Regulator Breakout - 3.3V](https://www.sparkfun.com/products/18357)                                        | 1   | [7.30$ (Core Electronics)](https://core-electronics.com.au/sparkfun-babybuck-regulator-breakout-3-3v-ap63203.html)                                      | 7.30$      |
| PWM 5V Logic Level Shifter for LED Strip | [Logic Level Converter BiDirectional](https://core-electronics.com.au/logic-level-converter-bidirectional.html)               | 1   | [3.05$ (Core Electronics)](https://core-electronics.com.au/logic-level-converter-bidirectional.html)                                                    | 3.05$      |
| Power Switch                             | [Rocker Switch (SPST)](https://core-electronics.com.au/rocker-switch-spst-round.html)                                         | 1   | [0.85$ (Core Electronics)](https://core-electronics.com.au/rocker-switch-spst-round.html)                                                               | 0.85$      |
| Player Switch                            | [16mm Panel Mount Momentary Pushbutton - Red](https://core-electronics.com.au/16mm-panel-mount-momentary-pushbutton-red.html) | 1   | [1.91$ (Core Electronics)](https://core-electronics.com.au/16mm-panel-mount-momentary-pushbutton-red.html)                                              | 1.91$      |
| Male-Female Jumpers                      | [Jumper Wire 20cm Ribbon M/F](https://core-electronics.com.au/male-female-jumper-wire-40-20cm.html)                           | 1   | [3.95$ (Core Electronics)](https://core-electronics.com.au/male-female-jumper-wire-40-20cm.html)                                                        | 3.95$      |
| Male-Male Jumpers                        | [Male to Male Jumper Wires](https://core-electronics.com.au/professional-male-to-male-jumper-wires-40-x-20cm.html)            | 1   | [7.30$ (Core Electronics)](https://core-electronics.com.au/professional-male-to-male-jumper-wires-40-x-20cm.html)                                       | 7.30$      |
| Male Headers                             | [Male Headers 2.56mm](https://core-electronics.com.au/header-male-pin-01x20.html)                                             | 2   | [0.35$ (Core Electronics)](https://core-electronics.com.au/header-male-pin-01x20.html)                                                                  | 0.70$      |
| Breadboard                               | [Breadboard - Mini Modular (Red)](https://www.sparkfun.com/products/12044)                                                    | 4   | [2.75$ (Core Electronics)](https://core-electronics.com.au/170-tie-point-mini-red-solderless-breadboard.html)                                           | 5.5$       |
| Shipping                                 |                                                                                                                               |     |                                                                                                                                                         | 10.20$     |
| Total Cost                               |                                                                                                                               |     |                                                                                                                                                         | 410.36$    |

The main microcontroller is an [STM32L432KC](https://www.st.com/en/microcontrollers-microprocessors/stm32l432kc.html)
on the [Nucleo-L432KC](https://www.st.com/en/evaluation-tools/nucleo-l432kc.html) used as the main board for the system.

## 6. Connecting the Board

The development board pins can be seen below:

<center>
  <img src="docs/board/pinout.jpg" width="500">
</center>

> [!note]
> The black button opposite the micro USB port is the reset button.

### 6.1 Display

Connect the following:

| Wire Colour | Display Function | Port | Configured As | Board Pin |
| ----------- | ---------------- | ---- | ------------- | --------- |
| Blue        | Data In          | PA7  | SPI1 MOSI     | A6        |
| Yellow      | Clock            | PA1  | SPI SCK       | A1        |
| Orange      | Chip Select      | PB0  | SPI_NSS       | D3        |
| Green       | Data / Command   | PA4  | GPIO          | A3        |
| White       | Reset            | PA0  | GPIO          | A0        |
| Red         | Power            | -    | 3.3V          | Vin       |
| Black       | Ground           | -    | 0V            | GND       |

### 6.2 Button

The button is expected to be on this pin:

| Port | Configured As | Board Pin |
| ---- | ------------- | --------- |
| PB1  | GPIO          | D6        |

Connect this pin to 3.3V through the button, and to GND through a 10kΩ pull-down
resistor.

### 6.3 RFID

Connect the TCA9548A multiplexer to the board:

| TCA9548A Pin | Function       | Port | Configured As | Board Pin |
| ------------ | -------------- | ---- | ------------- | --------- |
| VIN          | Power          | -    | 3.3V          | 3V3       |
| GND          | Ground         | -    | 0V            | GND       |
| SDA          | I2C data       | PB7  | I2C1 SDA      | D4        |
| SCL          | Clock          | PB6  | I2C1 SCL      | D5        |
| RST          | Reset          | -    |               | GND       |
| A0           | Address select | -    |               | GND       |
| A1           | Address select | -    |               | GND       |
| A2           | Address select | -    |               | GND       |

For every bank of 4 RFID readers, connect it to one of the ports on the TCA9548A
as follows:

| RFID Pin | Function | TCA9548A Pin                   |
| -------- | -------- | ------------------------------ |
| GND      | Ground   | GND                            |
| 3.3V     | Power    | VIN                            |
| SDA      | I2C data | SDX (For some bus number X)    |
| SCL      | Clock    | SCX (Same bus number as above) |

> [!note]
> Make sure readers connected to the same port have different I<sup>2</sup>C
> addresses from each other; i.e. set the dip switches in different positions.
> Since the PiicoDev modules support only 4 addresses, 4 is the maximum number
> of readers per port.

Additionally, connect the buzzer to the board like this:

| Buzzer pin | Function | Port | Board Pin |
| ---------- | -------- | ---- | --------- |
| Positive   | Buzzer   | PA8  | D9        |
| Negative   | Ground   | -    | GND       |

In the [`app.overlay`](/dracula/app.overlay) device tree, configure the `rfid`
nodes in the `&rfid_mux` tree. For example:

```dts
rfid_mux: mux@70 {
	reg = <0x70>;

	mux_i2c@0 {
		reg = <0>;

		rfid@2c {
			reg = <0x2c>;
			bearsink,room = "NHALL";
		};

		rfid@2d {
			reg = <0x2d>;
			bearsink,room = "TOMB";
		};

		rfid@2e {
			reg = <0x2e>;
			bearsink,room = "GUARDEDWAY";
		};
	};

	mux_i2c@1 {
		reg = <1>;

		rfid@2c {
			reg = <0x2c>;
			bearsink,room = "ALLEY";
		};

		rfid@2d {
			reg = <0x2d>;
			bearsink,room = "BONEPIT";
		};
	};
};
```

> [!note]
> Many device tree properties are omitted from the above example for brevity's
> sake, but are necessary for proper functioning. See the actual `app.overlay`
> source for details.

The `mux_i2c@X` nodes represent SDX/SCX pin pairs on the TCA9548A. The `X` in
the node name and the `reg = <X>;` property must be set to the port number of
the pins.

These nodes can then each contain up to four `rfid@XX` nodes, representing RFID
readers. The `XX` in the node name and the `reg = <0xXX>;` property must be set
to the I<sup>2</sup>C bus address of the reader, as set by the dip switches.

Then, in the `rfid@XX` nodes, set the `bearsink,room` property to the room name
(as in [`enum RoomName`](/dracula/src/room.h) value corresponding to the room
each reader represents

## 7. Project Overview

There five top level components:
- **RFID Sensors** - The driver provides the information about each sensor and
  any tokens placed on top of them. This is implemented at [`rfid.cpp` /
  `rfid.h`](/dracula/src/rfid.cpp). The RFID library at
  [`MFRC522_I2C.cpp`](/dracula/src/MFRC522_I2C.cpp) is from
  [arozcan/MFRC522-I2C-Library](https://github.com/arozcan/MFRC522-I2C-Library)
  and has been adapted to use Zephyr APIs.
- **LED Strip** - Used to display user.
- **OLED Display** - Provides complex information to the users and information
  unable to be conveyed by the LEDs or through game mechanics. Implemented by
  [`display.c` / `display.h`](/dracula/src/display.h).
- **Dracula AI** - Determines the move that Dracula makes.
- **Game Logic** - Combines the other components together with game logic
  including condition checks and logical game flow. Implemented by [`dracula.h`
  / `dracula.c`](/dracula/src/dracula.h).

The entrypoint to the game logic is located at
[`dracula/src/main.c`](dracula/src/main.c).

## 8. Repository Structure

The majority of our original work is contained in the `assets/`, `dracula/`, and
`prototypes/` directories.

- [`.devcontainer`](/.devcontainer)
  - [`devcontainer.json`](/.devcontainer/devcontainer.json) - Development container configuration file.
  - [`dockerfile`](/.devcontainer/dockerfile) - Dockerfile defining the zephyr development environment.
  - [`entrypoint.sh`](/.devcontainer/entrypoint.sh) - Script run in every terminal connecting to the devcontainer.
- [`.vscode`](/.vscode)
  - [`.vscode/c_cpp_properties.json`](/.vscode/c_cpp_properties.json) - Intellisense configuration for zephyr files and source code.
  - [`.vscode/extensions.json`](/.vscode/extensions.json) - Extensions required to install for VSCode, to support the development container and C intellisense.
  - [`.vscode/launch.json`](/.vscode/launch.json) - Debugging configurations when pressing F5.
  - [`.vscode/settings.json`](/.vscode/settings.json) - General VSCode project settings.
  - [`.vscode/tasks.json`](/.vscode/tasks.json) - Terminal commands to run for repetitive actions, such as building and flashing.
- [`assets`](/assets) - Resources files that are not including directly in code.
  - [`display`](/assets/display) - PNG files to convert to memory buffers for the display driver.
- [`dracula`](/dracula/) - Project files and source code.
  - [`prj.conf`](/dracula/prj.conf) - [KConfig](https://docs.zephyrproject.org/latest/build/kconfig/index.html) file that defines the enabled peripherals and microcontroller options. All available options can be found [here](https://docs.zephyrproject.org/2.7.5/reference/kconfig/index-all.html).
  - [`CMakeLists.txt`](/dracula/CMakeLists.txt) - CMake file defining the source files (globbed from [`src`](/dracula/src)) and zephyr as a project dependancy.
  - [`app.overlay`](/dracula/app.overlay) - A [DeviceTree](https://docs.zephyrproject.org/latest/build/dts/intro-scope-purpose.html) overlay file that defines and configures the microcontroller hardware including SPI, I2C, GPIO and PWM, to communicate with display, rfid and led strip.
  - [`src`](/dracula/src) - Source code for the project. *This directory contains the large majority of our original work.*
    - [`ai.c`](/dracula/src/ai.c) / [`ai.h`](/dracula/src/ai.h) - Implementation of the dracula AI.
    - [`buzzer.c`](/dracula/src/buzzer.c) / [`buzzer.h`](/dracula/src/buzzer.h) - Dedicated thread controlling the buzzer. (Commands are given to it by the RFID thread)
    - [`display.c`](/dracula/src/display.c) / [`display.h`](/dracula/src/display.h) - Driver for the [OLED display module](https://core-electronics.com.au/242inch-oled-display-module-128x64px.html) that uses the [`SSD1309`](https://www.hpinfotech.ro/SSD1309.pdf) IC interface.
    - [`dracula.c`](/dracula/src/dracula.c) / [`dracula.h`](/dracula/src/dracula.h) - Core game logic implementation.
    - [`font.c`](/dracula/src/font.c) / [`font.h`](/dracula/src/font.h) - Interface for displaying strings on the display.
    - [`MRFC522_I2C.cpp`](/dracula/src/MRFC522_I2C.c) / [`MRFC522_I2C.h`](/dracula/src/MRFC522_I2C.h) - Library for interfacing with the [RFID modules](https://core-electronics.com.au/piicodev-rfid-module.html). From [arozcan/MFRC522-I2C-Library](https://github.com/arozcan/MFRC522-I2C-Library), but with slight modifications to run on Zephyr.
    - [`rfid.cpp`](/dracula/src/rfid.cpp) / [`rfid.h`](/dracula/src/rfid.h) - Driver coordinating the multiple RFID modules and keeping track of token locations
    - [`room.c`](/dracula/src/room.c) / [`room.h`](/dracula/src/room.h) - Functions and data structures for manipulating arrays of rooms
    - [`ui.c`](/dracula/src/ui.c) / [`ui.h`](/dracula/src/ui.h) - User interface implementation for Dracula including splash screen, game events, and error conditions built on the display driver.
- [`prototypes`](/prototypes) - Prototypes of AI and game logic, *not* compiled for or sent to the board.
    - [`debug`](/prototypes/debug) - A version of the C game logic that uses the ai and room code that is loaded onto the device, but does IO in the terminal. Useful for debugging the ai.
    - [`python`](/prototypes/python) - Some python prototyping of the game rules.
    - [`quantum_dracula`](/prototypes/quantum_dracula) - The Haskell prototype of the Quantum Dracula ai.
- [`scripts`](/scripts)
  - [`build.sh`](/scripts/build.sh) - Builds the source file.
  - [`clean.sh`](/scripts/clean.sh) - Deletes the build folder of the last compilation.
  - [`convert_png.py`](/scripts/convert_png.py) - Converts PNG files to memory buffers able to be written to the display in code.
  - [`flash.sh`](/scripts/flash.sh) - Flashes the board with the most recent build.
  - [`format.sh`](/scripts/format.sh) - Autoformats source files.
  - [`jlink_postinit.sh`](/scripts/jlink_postinit.sh) - To be run inside the container after running because setup requires advanced privileges, to allow debugging.
  - [`screen.sh`](/scripts/screen.sh) - Connects to the serial port on the development board to monitor system logs.
  - [`source_zephyr.sh`](/scripts/source_zephyr.sh) - Initialises zephyr environment variables from the SDK location.
  - [`usbip_wsl_attach.sh`](/scripts/usbip_wsl_attach.sh) - Automated script to detect and attach the to microcontroller communications.
  - [`west_update.sh`](/scripts/west_update.sh) -
- [`.gitattributes`](/.gitattributes) - Ensures source files and bash scripts maintain linux LF line endings.
- [`.gitignore`](/.gitignore) - Ignore file for C and python to prevent build files being committed to git.
- [`README.md`](/README.md) - Project documentation.

## 9. Using the Display

The display has 8 rows, each with 128 columns. Each row is 8 pixels tall, and
each of the 8 bits in a byte written to the display sets the 8 pixel states from
LSB to MSH at a given row and column.

Since creating data buffers for such a structure is difficult by hand, A python
script is supplied under [`scripts/convert_png.py`](/scripts/convert_png.py)
that takes a png file and produces a memory buffer usable with the
[`dracula/src/display.h`](/dracula/src/display.h) API.

The first step is to design a png file suitable for the display. The PNG
requires:
- The maximum pixel dimensions must be 64 pixels high, 128 pixels wide.
- The height of the png must be a multiple of 8, since the display writes pixel
  data in sets of 8.
- Since the OLED pixels can only be on or off, only pure black white images
  should be converted.

> [!warning]
> The produced buffer should not be stored in global static memory, that would
> use up valuable RAM. Instead, each buffer should be defined in it's own
> function, and therefore retrieved from program memory when required.

## 10. Privacy and Ethical considerations

### 10.1 Data Security and Privacy

The only user data collected by the
game board is the specific moves made by players in the context of a game. This
data is not personally-identifying information, and is stored exclusively in
volatile memory, so is erased when the power switch is flipped off.

The RFID tokens, being the only loose parts, are susceptible to being lost by
players, which would make the game unplayable were they to lose e.g. a player
token. This is why the tokens use the standard NDEF format to store data.
Players can reprogram tokens, or create new tokens, using any NFC tag reader/
writer (for example, a smartphone app). See the code that reads tokens in
[`rfid.cpp`](/dracula/src/rfid.cpp#L142-L214).

Since the tokens encode URLs, they can also be scanned by players' phones to
take them to this repository. This is facilitated by a static redirect server,
and players' IP addresses and User Agent strings are shared with this redirect
server and with GitHub (after the redirect). Given this process occurs for
every website they visit, we do not believe this poses any additional risk to
users.

### 10.2 Ethics

An ethical product should be as accessible as possible. This game aims to be as
accessible as possible in a few ways:
- **Avoidance of text:** The game avoids communicating important information by
  text as much as possible, to make it accessible to players of varying literacy.
  This includes (for example) younger players.
- **Multi-sensory feedback:** The game provides both visual and auditory feedback
  for player actions, increasing accessibility for players of low vision and
  hearing. The game's core interactions were also designed to avoid the need to
  see small details.
- **Colour palette:** The game's colour palette was selected to be visually 
  distinctive to players with all three types of colourblindness.

## 11. Issues

The following are issues that we are currently aware of:

- If players want to end their turn after moving and without using a resource,
  they have to press the end turn button twice.

## 12. Additional Resources

- Prototypes for the game logic and the AI may be found in
  [`prototypes`](/prototypes/)
- [Weekly Notes](/docs/notes/)
- [Github Wiki](https://github.com/alegs-a/bears-ink/wiki)
