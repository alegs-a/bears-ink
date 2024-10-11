CC = gcc
CFLAGS = -Wall -Werror -std=gnu99 -pedantic -g
.PHONY: clean debug
.DEFAULT_GOAL := debug


debug: room.o ai.o debug.o ../../dracula/src/room.h ../../dracula/src/ai.h debug.h
	$(CC) $(CFLAGS) room.o ai.o debug.o -o debug

room.o: ../../dracula/src/room.c
	$(CC) $(CFLAGS) -c $<

ai.o: ../../dracula/src/ai.c
	$(CC) $(CFLAGS) -c $<

debug.o: debug.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o test run
