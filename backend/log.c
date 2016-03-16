#include <stdint.h>
#include "log.h"
#include "io.h"
#include "hash.h"
#include "heap.h"
#include "testcase.h"

// ================
// Global variables
// ================

log_t global_log;

// ==================
// Log initialisation
// ==================

void log_init()
{
  global_log.hash_table = (log_entry_t*) HASH_TABLE_BASE;
  global_log.num_collisions = 0;
  global_log.num_entries = 0;
  for (int i = 0; i < HASH_TABLE_SIZE; i++)
    global_log.hash_table[i].count = 0;
  global_log.headstarts = (uint64_t*) HEADSTARTS_BASE;
  for (int i = 0; i < NUM_HEADSTARTS; i++)
    global_log.headstarts[i] = 0;
}

// =====================
// Logging a 'headstart'
// =====================

// A 'headstart' is a period of time for which process 0 has already
// been executing a test case before another process has started
// executing it.  (A negative headstart meaning that the other process
// started first.)  We maintain a histogram for headstarts that lie
// within small range either side of '0'.

void log_headstart()
{
  for (int i = 1; i < NUM_PROCESSES; i++) {
    // Calculate head-start of process 0 relative to this process
    int64_t headstart = (int64_t) test.start_times[i] - 
                        (int64_t) test.start_times[0];
    headstart += NUM_HEADSTARTS/2;
    if (headstart >= 0 && headstart < NUM_HEADSTARTS)
      global_log.headstarts[headstart]++;
  }
}

// ====================
// Logging an 'outcome'
// ====================

// Hash table based on code from litmus tool by Luc Maranget and
// Susmit Sarkar.  See <http://diy.inria.fr>.

void log_add_outcome()
{
  uint32_t h = hash( (uint8_t *) test.outcome
                   , LEN_OUTCOME*sizeof(var_t)
                   , 0 );
  h = h % HASH_TABLE_SIZE;
  for (int i = 0; i < HASH_TABLE_SIZE; i++) {
    log_entry_t *entry = global_log.hash_table + h;
    if (entry->count == 0) { /* New entry */
      for (int j = 0; j < LEN_OUTCOME; j++)
        entry->outcome[j] = test.outcome[j];
      entry->count = 1;
      global_log.num_entries++;
      if (i != 0) global_log.num_collisions++;
      return;
    }
    else {
      int found = 1;
      for (int j = 0; j < LEN_OUTCOME; j++)
        if (entry->outcome[j] != test.outcome[j]) { found = 0; break; }
      if (found) {
        entry->count++;
        return;
      }
    }
    h = (h+1) % HASH_TABLE_SIZE;
  }
  put_string("Hash table is full\n");
}

// ================
// Updating the log
// ================


void log_update()
{
  log_headstart();
  log_add_outcome();
}

// ==================
// Displaying the log
// ==================

void log_display_outcome()
{
  char* outcome_names[] = OUTCOME_NAMES;
  var_t sought[] = OUTCOME_SOUGHT;
  int found = 0;
  for (int i = 0; i < HASH_TABLE_SIZE; i++) {
    log_entry_t *entry = global_log.hash_table + i;
    if (entry->count != 0) {
      put_uint64(entry->count);
      put_string(": ");
      int got = 1;
      for (int j = 0; j < LEN_OUTCOME; j++) {
        if (entry->outcome[j] != sought[j]) got = 0;
        put_string(outcome_names[j]);
        put_string("=");
        put_uint32((uint32_t) entry->outcome[j]);
        put_string(" ");
      }
      if (got == 1) found = 1;
      put_string("\n");
    }
  }
  put_string(found ? "OBSERVED\n" : "NOT OBSERVED\n");
}

void log_display_headstart()
{
  int64_t mid = NUM_HEADSTARTS/2;
  put_string("\nHeadstarts:\n");
  for (int i = 0; i < NUM_HEADSTARTS; i++) {
    put_int64(i-mid);
    put_string(": ");
    put_uint64(global_log.headstarts[i]);
    put_string("\n");
  }
}

void log_display()
{
  log_display_outcome();
  #ifdef SHOW_HEADSTARTS
  log_display_headstart();
  #endif
}
