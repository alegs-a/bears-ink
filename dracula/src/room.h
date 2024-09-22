#include <stddef.h>



///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                                  TYPES                                  ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////


#ifndef ROOM_TYPES
#define ROOM_TYPES

// The names of all of the rooms on the board
enum RoomName {
    NHALL,
    TOMB,
    GUARDEDWAY,
    GALLERY,
    ALLEY,
    BONEPIT,
    ENTRANCE,
    VENT,
    DUNGEON,
    DINING,
    LIBRARY,
    CRYPT,
    PASSAGE,
    CHAPEL,
    NEST,
    BATHROOM,
    CANAL,
    STAIRCASE,
    CELLAR,
    SHALL,
    BALLROOM,
    MAXIMUM_ROOM
};

// Room type. Prefer to pass this around over room names so that we can use the
// structure of the map
typedef struct {
    enum RoomName room;
    struct RoomBuffer *adjacent;
} Room;

// Buffer of rooms
struct RoomBuffer {
    Room *rooms; // array of rooms
    size_t length; // number of rooms in the buffer.
};

#endif

#define NUM_ROOMS 21



///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                         FUNCTION PROTOTYPES                             ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////


/*
 * Add the room to the room buffer if it is not there already.
 *
 * buff - the buffer to which to add the room. Assume that enough memory is
 *  allocated for an extra room.
 * room - the room to be added
 *
 */
void add_no_duplicate(struct RoomBuffer *const buff, const Room room);

/*
 * Return the index of the given room in the room buffer. If it is not in the
 * buffer, return -1.
 *
 * This function has NO side effects
 */
int contains_room(const struct RoomBuffer buff, const Room room);

/*
 * Create a new room buffer that is a copy of buff. So that the memory of the
 * new buffer is disjoint to that of buff, pass in an array arr that can be
 * used as the rooms array in the room buffer, and copy the values over.
 */
struct RoomBuffer room_buffer_copy(const struct RoomBuffer buff, Room *const arr);
