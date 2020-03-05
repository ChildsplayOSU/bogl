# Spiel-Lang

## Installation
1. Install [the haskell tool stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Install [doctest](https://hackage.haskell.org/package/doctest) for haskell to run the test suite
   * These tests can be run with `stack test`

## Running

To run spiel first you'll need to start it up. Inside the project's top-level directory or deeper type the following into the terminal:
```bash
stack ghci
```

If everything went well an instance of Spiel-Lang should be running.

To test it out, try one of the provided examples. This can be done as follows:
```
>runPrototype "examples/example1.bgl"
```
This will host an instance of the "game" at an IP address which is reported to you. Open it up in a web broswer and you will see a text box, which is a REPL. You can try example expressions such as: 

```
or(True, False)
let x = 2 in 40 + x 
```
You can also try evaluating expressions using the functions in the example1.bgl file: 
```
succ(99) 
```


This wil start up one of the examples provided with the code. Keep in mind the location to the file is relative to your current working directory.

For examples of actual games, try the following:
```
>runPrototype "examples/Notakto.bgl"
>runPrototype "examples/TicTacToe.bgl"
```
## Playing

Since there is no predefined entry function, playing a game is dependent on how the individual program was written. To play Notakto, for example, you can call the gameLoop function with the board called empty. 
```
gameLoop(empty)
```

To see an example of type errors, you can run:
```
>runFile "examples/typeerrors.bgl"
```

## Caveats 

This is a work in progress. Note that there are bugs and the syntax of the examples does not quite match the specified syntax. We have refined the syntax a lot and we are still catching the implementation up. Additionally, the type and syntax errors are mostly in debugging-friendly form rather than being tailored for easy understanding. 
