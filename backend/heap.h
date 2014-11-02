#ifndef _HEAP_H_
#define _HEAP_H_

#include "platform.h"

#define HEADSTARTS_BASE  HEAP_BASE
#define NUM_HEADSTARTS   32
#define HASH_TABLE_BASE  (HEAP_BASE+NUM_HEADSTARTS*sizeof(headstart_t))
#define HASH_TABLE_SIZE  128

#endif
