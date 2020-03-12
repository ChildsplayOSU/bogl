# Spiel-Lang

## Spiel is an implementation of the BoGL teaching language intended to model the domain of board game specification for use in middle school computer science and software engineering education  

## Installation
1. Install [the haskell tool stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

   * *optional:* Install [doctest](https://hackage.haskell.org/package/doctest) for haskell to run the test suite
   * These tests can be run with `stack test`

## Running

To run spiel first you'll need to start it up. Navigate to the spiel/examples directory and type the following into the terminal:

```bash
stack ghci
```
Note: in the future this process will be simplified with a packaged executable that can be started with a single click. 

If everything went well an instance of Spiel-Lang should be running.

To test it out, try one of the provided examples. This can be done as follows:
```
runPrototype "Example1.bgl"
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
runPrototype "Notakto.bgl"
runPrototype "TicTacToe.bgl"
runPrototype "ConnectFour.bgl"
```

For an example of type errors try this: 

```
runPrototype "TypeErrors.bgl"
```

Note: We still have a lot of room to improve type errors and parse errors. 

## Playing

Since there is no predefined entry function, playing a game is dependent on how the individual program was written. To play Notakto, for example, you can call the gameLoop function with the board called empty. 
```
gameLoop(empty)
```

## Caveats 

This is a work in progress. Note that there are bugs and the syntax of the examples does not quite match the specified syntax. We have refined the syntax a lot and we are still catching the implementation up. Additionally, the type and syntax errors are mostly in debugging-friendly form rather than being tailored for easy understanding. 
