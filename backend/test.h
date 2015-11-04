#ifndef _TEST_H_
#define _TEST_H_

#include <stdint.h>

// ==========
// Parameters 
// ==========

// Include automatically generated litmus test
#include "testcase.h"

// =====
// Types
// =====

// Shared variables in litmus test
typedef int64_t var_t;

typedef struct {
  // The outcome vector
  volatile var_t     outcome[LEN_OUTCOME];

  // For logging the start time of a test on each process
  volatile uint32_t  start_times[NUM_PROCESSES];

  // For inserting delays before a test on each process
  volatile uint32_t  delays[NUM_PROCESSES];

  // All the locations that a shared variable can reside
  volatile var_t*    locs;

  // The locations of the shared variables for the current test
  volatile var_t*    vars[NUM_VARS];
} test_t;

// =======
// Globals
// =======

extern test_t test;

// =========
// Functions
// =========

void test_init(uint32_t*);
void test_body(int pid);

#endif
