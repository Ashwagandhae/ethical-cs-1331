# Ethical CS 1331

All my Georgia Tech CS 1331 homeworks and programming exercises rewritten in the functional language [Haskell](https://www.haskell.org/) and functional depedently-typed language [Idris](https://www.google.com/search?q=idris2&sourceid=chrome&ie=UTF-8), making each of the programms at least seven times more ethical.

These assignments were inteded to be written in Java 😔, and included many tasks heavily intertwined with objected-oriented ideas, which made translating them into functional languages even more fun!

## Running

Each program is located in the `hw*` and `pe*` directories (for `homework` and `programming exercises`, respectively). First, navigate to the directory of the program you want to run (with `cd <directory>`), then:

### Haskell programs

If the program is written in Haskell (you can tell it's written in Haskell if the directory contains a `*.cabal` file):

1. Install [GHCup](https://www.haskell.org/ghcup/) to install `cabal` and other Haskell stuff.
2. Run

```bash
cabal run
```

### Idris programs

If the program is written in Idris (you can tell it's written in Idris if the directory contains a `*.ipkg` file):

1. Install [idris2](https://github.com/idris-lang/Idris2/blob/main/INSTALL.md), and install [pack](https://github.com/stefan-hoeck/idris2-pack).
2. Run

```bash
pack run
```
