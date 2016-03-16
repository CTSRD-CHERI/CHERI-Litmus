OUTPUT_ARCH( "riscv" )

BOOT_MEM  = 0x0000200;
DRAM_BASE = 0x0100000;
DRAM_TOP  = 0x4000000;

SECTIONS
{
  . = BOOT_MEM;

  .text   : { *.o(.text) }
  .bss    : { *.o(.bss) }
  .data   : { *.o(.data) }
  .rodata : { *.o(.rodata*) }
}
