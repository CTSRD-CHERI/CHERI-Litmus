L3MIPS=/local/scratch/mn416/l3mips/l3mips

if [ "$1" = "t" ]; then
  TRACE="--trace 2"
else
  TRACE=""
fi

$L3MIPS $TRACE --nbcore 2 --uart-in /dev/null --format raw main.bin
