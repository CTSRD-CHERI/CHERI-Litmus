#ifndef _RAND_H_
#define _RAND_H_

#include <stdint.h>

int rand_bit(uint32_t *seed);
uint32_t rand_k(uint32_t *seed, uint32_t upper);

#endif
