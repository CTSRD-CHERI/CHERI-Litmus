PLATFORM="rocket"

CC="riscv64-unknown-elf-gcc"
AS="riscv64-unknown-elf-as"
LD="riscv64-unknown-elf-ld"
OBJCOPY="riscv64-unknown-elf-objcopy"
OBJDUMP="riscv64-unknown-elf-objdump"

OPT="-O2"
CFLAGS="$OPT -I."
LDFLAGS="-G 0 -T $PLATFORM/$PLATFORM.ld"

CFILES="main io log hash rand riscv/arch rocket/platform test"
OFILES=""
for F in $CFILES
do
  OFILES="$OFILES `basename $F.o`"
  $CC $CFLAGS -std=gnu99 -Wall -c -o `basename $F.o` $F.c
done

$AS -o entry.o rocket/entry.s
$LD $LDFLAGS -o main.elf entry.o $OFILES
elf2hex 16 65536 main.elf > main.hex
