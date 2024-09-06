## Dracula

Welcome to the core repository of dracula, an interactive and collaborative
technology enhanced board game of hidden information. In Dracula, players must
race to track down and stake Dracula before they are all eliminated. On each
turn, a player can move to a new room on a castle board, cast light to block
Dracula from moving, throw garlic to detect him, and throw stakes to attack him.
Each board game piece is RFID tagged to be read by RFID readers in the board.
After all players turns, the game's computer uses the RFID information to
automate Dracula's actions and display game information to the players using an
LCD screen and LEDs, that provides an engaging user experience. Will the players
be able to take down Dracula in time?

## Development

The development environment of Dracula is driven using
[Docker](https://www.docker.com/) and VSCode. Please reopen this folder in a
developement container to get started, or alternatively use a local
[Zephyr RTOS](https://www.zephyrproject.org/) installation that is used for the
operating system. Zephyr was selected to enable concurrent
realtime processing of RFID sensing, game logic and display output. The
microcontroller is an [STM432KC](https://www.st.com/en/microcontrollers-microprocessors/stm32l432kc.html) on a
[Nucleo-L432KC](https://www.st.com/en/evaluation-tools/nucleo-l432kc.html).

## File Structure

The main code project is located in [`dracula`](/dracula/).
- The entrypoint to the game logic is located at [dracular/src/main.c](dracula/src/main.c).
- The game logic is handled by [`dracula.h` / `dracula.c`](/dracula/src/dracula.h)
- The display implementation is at [`display.c` / `display.h`](/dracula/src/display.h)
- The RFID implementation is at [`rfid.c` / `rfid.h`](/dracula/src/rfid.h)

Additionally, prototypes for the game logic and the AI may be found in [`prototypes`](/prototypes/)

## Documentation

The documentation is split into several locations
- [Weekly Notes](/docs/notes/)
- [Github Wiki](https://github.com/alegs-a/bears-ink/wiki)
