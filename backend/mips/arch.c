#include "arch.h"
#include "rand.h"
#include "testcase.h"

// Code specific to MIPS64 processors

// Instances ==================================================================

inline int get_core_id()
{
  uint64_t x;
  asm volatile("dmfc0 %0, $15, 6": "=r" (x));
  return (int) (x & 0xffff);
}

inline int get_num_cores()
{
  uint64_t x;
  asm volatile("dmfc0 %0, $15, 6": "=r" (x));
  x >>= 16;
  x++;
  return (int) (x & 0xffff);
}

inline int get_thread_id()
{
  uint64_t x;
  asm volatile("dmfc0 %0, $15, 7": "=r" (x));
  return (int) (x & 0xffff);
}

inline int get_threads_per_core()
{
  uint64_t x;
  asm volatile("dmfc0 %0, $15, 7": "=r" (x));
  x >>= 16;
  x++;
  return (int) (x & 0xffff);
}

int arch_get_process_id()
{
  return get_threads_per_core() * get_core_id() + get_thread_id();
}

int arch_get_num_processes()
{
  return get_threads_per_core() * get_num_cores();
}

// Hardware counter ===========================================================

uint32_t arch_get_counter()
{
  uint64_t x;
  asm volatile("dmfc0 %0, $9": "=r" (x));
  return (uint32_t) x;
}

// Barrier synchronisation ====================================================

// Shared variables
static volatile uint64_t barrier1 = 0;
static volatile uint64_t barrier2 = 0;

void barrier_wait(
    volatile uint64_t* barrier
  , uint64_t incr_amount
  , uint64_t reach
  )
{
  asm volatile (
      "1:                                \n"
      "lld    $8, 0(%0)                  \n"
      "daddu  $8, $8, %1                 \n"
      "scd    $8, 0(%0)                  \n"
      "beqz   $8, 1b                     \n"
      "2:                                \n"
      "ld     $8, 0(%0)                  \n"
      "bne    $8, %2, 2b                 \n"
      "sync                              \n"
  : /* output operands */
  : /* input operands */
    "r"(barrier),
    "r"(incr_amount),
    "r"(reach)
  : /* clobbered registers */
    "$8"
  );
}

void arch_barrier_up()
{
  barrier_wait(&barrier1, 1, NUM_PROCESSES);
  barrier_wait(&barrier2, 1, NUM_PROCESSES);
}

void arch_barrier_down()
{
  barrier_wait(&barrier1, -1, 0);
  barrier_wait(&barrier2, -1, 0);
}
