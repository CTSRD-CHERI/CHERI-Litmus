#include "io.h"
#include "platform.h"

// ===============
// Output routines
// ===============

void put_string(char* s)
{
  while (*s) { put_char(*s); s++; }
}

void put_uint32(uint32_t i)
{
  put_uint64((uint64_t) i);
}

void put_uint64(uint64_t i)
{
  char str[32];
  int n = 0;
  while (i > 0) { str[n] = '0' + (i % 10); i /= 10; n++; }
  if (n == 0)
    put_char('0');
  else
    while (n > 0) { n--; put_char(str[n]); }
}

void put_int64(int64_t i)
{
  if (i < 0) {
    put_char('-');
    i = -i;
  }
  put_uint64 ((uint64_t) i);
}

void halt()
{
  put_string("Halted\n");
  while(1);
}
