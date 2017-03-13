PLATFORM="beri"
ARCH="-DARCH_MIPS"

CC="mips-linux-gnu-gcc"
AS="mips-linux-gnu-as"
LD="mips-linux-gnu-ld"
OBJCOPY="mips-linux-gnu-objcopy"
OBJDUMP="mips-linux-gnu-objdump"

OPT="-O2"
CFLAGS="-EB -march=mips64 -mabi=64 -G 0 -ggdb $OPT -I."
LDFLAGS="-EB -G 0 -T $PLATFORM/$PLATFORM.ld -m elf64btsmip"

CFILES="main io log hash rand mips/arch beri/platform test"
OFILES=""
for F in $CFILES
do
  OFILES="$OFILES `basename $F.o`"
  $CC $CFLAGS -fno-stack-protector -std=gnu99 -Wall $ARCH -c -o `basename $F.o` $F.c
done

$AS $CFLAGS -o entry.o beri/entry.s
$LD $LDFLAGS -o main.elf entry.o $OFILES
$OBJCOPY -S -O binary main.elf main.bin
