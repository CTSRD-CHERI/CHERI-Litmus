#ifndef _HASH_H_
#define _HASH_H_

#include <stdint.h>

// Hash function for a key represented as a byte array
uint32_t hash(uint8_t* k , uint32_t length, uint32_t initval);

#endif
