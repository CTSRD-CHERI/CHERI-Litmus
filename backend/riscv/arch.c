#include "arch.h"
#include "rand.h"
#include "io.h"
#include "testcase.h"

// Code specific to RISCV64

// Hardware thread id =========================================================

int arch_get_process_id()
{
  uint64_t x;
  asm volatile("csrr %0, 0xf15" : "=r" (x));
  return (int) x;
}

// Hardware counter ===========================================================

uint32_t arch_get_counter()
{
  uint64_t x;
  //asm volatile("csrr %0, 0xf01" : "=r" (x));
  asm volatile("csrr %0, 0xf00" : "=r" (x));
  return (int) x;
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
      //"1:                                \n"
      //"lr.d   a0, 0(%0)                  \n"
      //"add    a0, a0, %1                 \n"
      //"sc.d   a0, a0, 0(%0)              \n"
      //"bnez   a0, 1b                     \n"
      "amoadd.d a0, %1, (%0)             \n"
      "2:                                \n"
      "ld     a0, 0(%0)                  \n"
      "bne    a0, %2, 2b                 \n"
      "fence                             \n"
  : /* output operands */
  : /* input operands */
    "r"(barrier),
    "r"(incr_amount),
    "r"(reach)
  : /* clobbered registers */
    "a0"
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
