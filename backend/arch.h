#ifndef _ARCH_H_
#define _ARCH_H_

#include <stdint.h>

// Interface to CPU architecture specific code

void arch_barrier_up();
void arch_barrier_down();

int arch_get_process_id();
uint32_t arch_get_counter();

#endif
