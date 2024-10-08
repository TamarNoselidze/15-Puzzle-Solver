# 15-Puzzle Solver

This Haskell program solves the 15-puzzle game using the A* search algorithm. 
It takes either a board configuration from a file or generates a random puzzle.

## Requirements

To run this project, you need to have the following installed:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)


## Files

- `15-solver.hs`: The main source code of the program.
- `15-solver`: Compiled binary.
- `15-solver.hi`: Haskell interface file (generated during compilation).
- `15-solver.o`: Compiled object file (generated during compilation).


## How to Build

1. Open a terminal and navigate to the project directory.
2. Run the following command to compile the program:
   ```bash
   ghc 15-solver.hs


## How to Run
### From a File

- To solve a puzzle from a file, the file should contain the board configuration. The format should be:

```
1   2   3   4
5   6   7   8
9   10  11  12
13  14  15  0
```

- Run the program with the filename as an argument:

`./15-solver <filename>`

### Generate a Random Puzzle

To generate a random puzzle, use the `-r` flag followed by the number of random moves:

`./15-solver -r <num-random-moves>`

Example: `./15-solver puzzle.txt`   or `./15-solver -r 20`


## Output

The program will output the initial state of the board, the optimal solution steps, the number of explored states, 
and the number of moves required to solve the puzzle.
