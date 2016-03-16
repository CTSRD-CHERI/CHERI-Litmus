This is a very lightweight clone of the [Litmus
Tool](http://diy.inria.fr/) for running litmus tests (tiny concurrent
programs) on bare-metal hardware, i.e. with no OS or POSIX
implementation required.  

It lets us run litmus tests earlier in the design process, on hardware
that isn't yet capable of booting an OS.  The absence of an OS also
allows litmus tests to run on an RTL simulator in reasonable time,
simplifying debugging.

On the downside, the random perturbations due to OS background
activities, which can surely affect the observable outcomes of a
litmus test, are no longer present.  I believe such effects are relied
upon in the original Litmus Tool.  In future, it would be useful to
extend our tool to inject random memory noise to improve variability.

The original [Litmus Tool](http://diy.inria.fr/) is far more advanced,
and should definitely be used in addition to (or instead of)
CHERI-Litmus as soon as the hardware is sufficiently capable.

CHERI-Litmus currently supports two architectures:

1. [CHERI](https://cheri-cpu.org/) (MIPS64 ISA).

2. [Rocket Chip](https://github.com/ucb-bar/rocket-chip) (RISCV ISA).

I had thought about renaming CHERI-Litmus to Bare-Litmus to reflect
that more than one arch is supported, but it would probably break lots
of existing build scripts for CHERI.

# Directory layout:

  * *backend*: the infrastructure to run a generic litmus test

  * *frontend*: convert litmus files into C which when compiled with the
    backend produce an executable.

  * *tests*: sets of litmus files produced by the [diy](http://diy.inria.fr/)
    tool.

  * *binaries*: the make script here can take a path to a set of litmus
    files and produce a set of binaries.

# Instructions:

To build the frontend (which converts litmus files to C files), type
`./make.sh` in the `frontend` directory.  The dependencies for
building the frontend are:

  * `sudo apt-get install ghc`

  * `sudo apt-get install haskell-platform`

To generate bare-metal binaries:

  * for CHERI, type `./make.sh ../tests/mips/general`
    in the `binaries` directory.

  * for RISCV, type `./make-riscv.sh ../tests/riscv/`
    in the `binaries` directory.

The tests are standlone, requiring no input, and emit information
about whether the behaviour described by the test was observed or not.
