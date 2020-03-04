# Spiel-Lang

## Installation
1. Install [the haskell tool stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Set up any dependencies that you do not have:
   1. `sudo cabal update`
   2. `sudo cabal install haskeline`

3. Install [doctest](https://hackage.haskell.org/package/doctest) for haskell to run the test suite
   * These tests can be run with `stack test`

## Running

To run spiel first you'll need to start it up. Make sure you are inside the top level Spiel-Lang directory or deeper. And then type the following into the terminal.
```bash
stack ghci
```

If everything went well an instance of Spiel-Lang should be running.

To test out a game, try one of the provided examples. This can be done as follows:
```
>runFile "examples/example1.bgl"
```
This wil start up one of the examples provided with the code. Keep in mind the location to the file is relative to your current working directory.

For examples of actual games, try the following:
```
>runFile "examples/Notakto.bgl"
>runFile "examples/TicTacToe2.bgl"
```

The following need fixing:

~~>runFile "examples/TicTacToe.bgl"~~

~~>runFile "examples/ConnectFour.bgl"~~

## Playing

To play any game you can call the gameLoop that starts it. In this example let's assume we have an empty board to start with as well. That would look like this:
```
gameLoop(empty)
```

Playing the game, for input values, involves putting in 2 values, 1st is the x, and 2nd is the y. After that, the resulting board equation will be printed to the screen.
```
1
2
```


To see an example for type errors, you can run:
```
>runFile "examples/typeerrors.bgl"
```
