# bfhs

Yet another Brainf*ck interpreter in Haskell.

## Installation

```bash
git clone git@github.com:cezarguimaraes/bfhs.git
cd bfhs
cabal install
```

## Interpreter details

- Ignores any invalid characters as comments.
- Max value is by default `255` and values wrap back to 0. Disable wrapping
`--no-value-wrap` or change its maximum to `x-1` with `-mx` (i.e, wrapping
is done `modulo x`).
- Maximum `30000` cells on the "tape" by default and wraps by default. Wrapping
can be disabled and size of the tape can also be changed via options. Similar to
cells. Check [usage](#usage) below.
- State of a program after execution can be dumped with `-d`.

## Usage

```
bfhs
Usage: bfhs [opts] file
  -b                          TODO: Program file contains its input before a ! character.
  -c[30000]                   Number of cells available to the program. Ignored when --no-cell-wrap is set.
             --no-cell-wrap   Disables cell wrapping. Virtually infinite memory.
  -m[256]                     Maximum value of a cell. Ignored when --no-value-wrap is set.
             --no-value-wrap  Disables value wrapping, i.e allows negative values on memory
  -h         --help           Shows this message
  -d         --dump-state     Dump interpreter state at the end of execution
```

## Examples

- Fibonacci
    ```bash
    $ bfhs examples/fib.bf
    1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89
    ```

- Hello, World!
    ```bash
    $ bfhs examples/hello_world.bf
    Hello World!
    ```

- cat
    ```bash
    $ echo "ping" | bfhs examples/cat.bf
    ping
    ```

