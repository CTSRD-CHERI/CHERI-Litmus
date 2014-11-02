.set noreorder

  # Get core id
  mfc0   $t0, $15, 6
  andi   $t0, $t0, 0xffff

  # Get thread id and number of threads
  mfc0   $t1, $15, 7
  srl    $t2, $t1, 16
  addi   $t2, $t2, 1       # Num threads
  andi   $t1, $t1, 0xffff  # Thread id

  # Compute instance id
  mul    $t0, $t0, $t2     # Instance id = core id * num threads
  add    $t0, $t0, $t1     #             + thread id
  
  # Set stack pointer to DRAM_TOP - (instance id * 2K)
  dla    $sp, DRAM_TOP
  dmul   $t0, $t0, 0x800
  dsubu  $sp, $sp, $t0

  # Dump registers
  # mtc0   $zero, $26

  daddu $sp, $sp, -32    # Allocate 32 bytes of stack space

  dla   $t9, main
  jal   $t9
  nop

  # Simluation only ==========================
  dli $t0, 0x4000        # Delay to flush UART
  delay:
    daddi $t0, $t0, -1
  bnez $t0, delay
  nop

  mtc0  $zero, $23       # Terminate simulator
  # ==========================================

  b .
  nop
