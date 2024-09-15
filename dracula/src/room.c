#include "room.h"
#include <stdbool.h>


// Need to pass a pointer so that we can update the length of the buffer
void add_no_duplicate(struct RoomBuffer *const buff, const Room room) {
    if (!contains_room(*buff, room)) buff->rooms[buff->length++] = room;
}


bool contains_room(const struct RoomBuffer buff, const Room room) {
    for (int i = 0; i < buff.length; i++) {
        if (buff.rooms[i].room == room.room) return true;
    }
    return false;
}
