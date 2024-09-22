#ifndef ROOM_H
#define ROOM_H

#include <stddef.h>


///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                                  TYPES                                  ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////

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

#define NUM_ROOMS 21


///////////////////////////////////////////////////////////////////////////////
///                                                                         ///
///                         FUNCTION PROTOTYPES                             ///
///                                                                         ///
///////////////////////////////////////////////////////////////////////////////

/**
 * @brief Add the room to the room buffer if it is not there already.
 *
 * @param buff the buffer to which to add the room.
 * @room room the room to be added
 *
 * @note Assume that buff has enough memory allocated for an extra room.
 */
void add_no_duplicate(struct RoomBuffer *const buff, const Room room);

/**
 * @brief Give the index of the supplied room inside the room buffer
 *
 * @note This function has NO side effects
 *
 * @return the index of the room in the buffer if present. Otherwise, return -1
 */
int contains_room(const struct RoomBuffer buff, const Room room);

/**
 * @brief Create a new room buffer that is a copy of buff.
 *
 * @param buff the buffer of which to create a copy
 * @param arr the array to use as the underlying memory for the new buffer
 *
 * @return a new room buffer with the same length as and containing the same
 *  rooms as buff.
 *
 * @note The returned memory is entirely separate from that of buff (provided
 *  arr is entirely separate)
 */
struct RoomBuffer room_buffer_copy(
        const struct RoomBuffer buff,
        Room *const arr);

#endif // ROOM_H
