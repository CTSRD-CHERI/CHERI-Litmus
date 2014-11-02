#ifndef _LOG_H_
#define _LOG_H_

#include "test.h"

typedef struct {
  var_t outcome[LEN_OUTCOME];
  uint64_t count;
} log_entry_t;

typedef uint64_t headstart_t;

typedef struct {
  int num_collisions;
  int num_entries;
  log_entry_t* hash_table;
  headstart_t* headstarts;
} log_t;

void log_init();
void log_update();
void log_display();

#endif
