#ifndef _HEAP_H_
#define _HEAP_H_

#include "platform.h"
#include "log.h"
#include "testcase.h"

#define HEADSTARTS_BASE  HEAP_BASE
#define NUM_HEADSTARTS   32
#define HASH_TABLE_BASE  (HEAP_BASE+NUM_HEADSTARTS*sizeof(headstart_t))
#define HASH_TABLE_SIZE  128
#define LOCS_BASE        (HASH_TABLE_BASE+HASH_TABLE_SIZE*sizeof(log_entry_t))
#define LOCS_BASE_SIZE   NUM_LOCS

#endif
