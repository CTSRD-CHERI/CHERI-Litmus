// Code specific to the BERI/DE4 platform

#include "platform.h"

// =========
// DRAM Base
// =========

char const* HEAP_BASE = (char const*) 0x9800000000000000;

// ================
// JTAG UART output
// ================

volatile uint32_t *jtag_uart_ctrl = (volatile uint32_t *) 0x900000007f000004;
volatile char     *jtag_uart_data = (volatile char *)     0x900000007f000000;

void put_char(char c)
{
  // Wait until there is space left in the buffer
  while ((*jtag_uart_ctrl & 0xFFFF) == 0);
  // Emit a char
  *jtag_uart_data = c;
}
