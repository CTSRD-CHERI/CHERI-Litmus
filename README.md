# CHERI-Litmus

This is a very lightweight clone of the [Litmus
Tool](http://diy.inria.fr/) for running litmus tests (tiny concurrent
programs) on bare-metal hardware, i.e. with no OS or POSIX
implementation required.  

It lets us run litmus tests earlier in the design process, on hardware
that isn't yet capable of booting an OS.  It also allows litmus tests
to run on an RTL simulator in reasonable time, simplifying debugging.

On the downside, the random perturbations due to OS background
activities, which can surely affect the observable outcomes of a
litmus test, are no longer present.  Improving variability would be
worth investigating.

The original [Litmus Tool](http://diy.inria.fr/) is far more advanced,
and should definitely be used in addition to (or instead of)
CHERI-Litmus as soon as the hardware is sufficiently capable.

CHERI-Litmus currently supports two architectures:

1. [CHERI](http://www.cl.cam.ac.uk/research/security/ctsrd/cheri/) (MIPS64 ISA).

2. [Rocket Chip](https://github.com/ucb-bar/rocket-chip) (RISCV ISA).

I had thought about renaming CHERI-Litmus to Bare-Litmus to reflect
that more than one arch is supported, but it would probably break lots
of existing build scripts for CHERI.

## Directory layout

  * *backend*: the infrastructure to run a generic litmus test

  * *frontend*: convert litmus files into C which when compiled with the
    backend produce an executable.

  * *tests*: sets of litmus files produced by the [diy](http://diy.inria.fr/)
    tool.

  * *binaries*: the make script here can take a path to a set of litmus
    files and produce a set of binaries.

## Instructions

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
