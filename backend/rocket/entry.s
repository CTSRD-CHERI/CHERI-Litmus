HARTID=0xf10
TO_HOST=0x780

  # Get hardware thread id
  csrr a0, HARTID

  # Set stack pointer to DRAM_TOP - (id * 2K)
  la sp, DRAM_TOP
  sll a0, a0, 11
  sub sp, sp, a0

  # Allocate 32 bytes of stack space
  add sp, sp, -32

  # Jump-and-link to main
  jal main

  # Delay to flush UART in emulation
  li a0, 0xa000
delay:
  addi a0, a0, -1
  bnez a0, delay

  # Exit with success
  li a5, 1
  csrw TO_HOST, a5

  j .
