#include "room.h"
#include <stdbool.h>
#include <string.h>


inline void add_with_duplicate(struct RoomBuffer *buf, Room *room) {
    buf->rooms[buf->length++] = room;
}


// Need to pass a pointer so that we can update the length of the buffer
void add_no_duplicate(struct RoomBuffer *buff, Room *room) {
    if (!contains_room(*buff, room)) add_with_duplicate(buff, room);
}


void concat_no_duplicate(struct RoomBuffer *dst, const struct RoomBuffer src) {
    for (int i = 0; i < src.length; i++) {
        add_no_duplicate(dst, src.rooms[i]);
    }
}


void remove_if_present(struct RoomBuffer *buf, const Room *room) {
    int i = contains_room(*buf, room);
    while (i >= 0) {
        memmove(buf->rooms + i, buf->rooms + i + 1,
                sizeof(Room) * (buf->length - i - 1));
        buf->length--;
        i = contains_room(*buf, room);
    }
}


int contains_room(const struct RoomBuffer buff, const Room *room) {
    for (int i = 0; i < buff.length; i++) {
        if (buff.rooms[i] == room) return i;
    }
    return -1;
}


void room_buffer_copy(struct RoomBuffer *dst, const struct RoomBuffer src) {
    memcpy(dst->rooms, src.rooms, src.length * sizeof(Room));
    dst->length = src.length;
}


void remove_duplicate_rooms(struct RoomBuffer *buf) {
    // This algo is slightly cursed. It exploits buf and copy having the same
    // underlying memory.
    struct RoomBuffer copy;
    for (int i = 0; i < buf->length - 1; i++) {
        copy.rooms = buf->rooms + i + 1;
        copy.length = buf->length - i - 1;;
        remove_if_present(&copy, buf->rooms[i]);
        buf->length = copy.length + i + 1;
    }
}


struct RoomBuffer room_buffer_from(
        const struct RoomBuffer buff,
        Room **arr) {

    struct RoomBuffer res;
    res.rooms = arr;
    room_buffer_copy(&res, buff);
    return res;
}
