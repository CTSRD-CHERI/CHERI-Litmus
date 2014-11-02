
BOOT_MEM  = 0x9800000040000000;
DRAM_BASE = 0x9800000000000000;
DRAM_TOP  = BOOT_MEM - 0x2000008;

SECTIONS
{
  . = BOOT_MEM;

  .text   : { *.o(.text) }
  .bss    : { *.o(.bss) }
  .data   : { *.o(.data) }
  .rodata : { *.o(.rodata*) }

  _gp = _GLOBAL_OFFSET_TABLE_;

}
