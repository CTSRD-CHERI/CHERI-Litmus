#ifndef _PLATFORM_H_
#define _PLATFORM_H_

#include <stdint.h>

// Interface to platform-specific code

void flush();
void put_char(char c);
extern char const* HEAP_BASE;


#endif
