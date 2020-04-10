# Spiel-Lang

## Spiel is an implementation of an interpreter for the BoGL teaching language intended to model the domain of board game specification for use in middle school computer science and software engineering education  

This is the back end of the implementation which includes the interpreter itself and a server. To actually use the language you need the [front end](https://github.com/The-Code-In-Sheep-s-Clothing/Spiel-Front) which includes an editor and REPL. 

## Installation
1. Install [the haskell tool stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

   * *optional:* Install [doctest](https://hackage.haskell.org/package/doctest) for haskell to run the test suite
   * These tests can be run with `stack test`
   
   This will put a spielserver executable in your path which the front end can then use. 
## Examples 
To see an example of a program in the language look at the [examples](examples/) directory.
