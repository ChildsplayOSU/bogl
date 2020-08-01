# Spiel-Lang

[![Build Status](https://travis-ci.org/The-Code-In-Sheep-s-Clothing/Spiel-Lang.svg?branch=master)](https://travis-ci.org/The-Code-In-Sheep-s-Clothing/Spiel-Lang)

## Spiel is an implementation of an interpreter for the BoGL teaching language intended to model the domain of board game specification for use in middle school computer science and software engineering education

This is the back end of the implementation which includes the interpreter itself and a server. To actually use the language you need the [front end](https://github.com/The-Code-In-Sheep-s-Clothing/Spiel-Front) which includes an editor and REPL.

Note: Unless you intend to use this repository for development, we recommend accessing it via the link provided in our [informational website](https://the-code-in-sheep-s-clothing.github.io/Spiel-Lang/). In addition to an instance of the application itself, this website incldues up to date details on the language, how to get started with it, tutorials, and more.

## Installation
1. Clone this repository
2. Install [the haskell tool stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
3. Inside the project repository run `stack setup`
4. Run `stack install`. This will put a spielserver executable in your path which the front end can then use.
   * *optional:* Install [doctest](https://hackage.haskell.org/package/doctest) for haskell to run the test suite
   * These tests can be run with `stack test`
        
## Examples 
To see an example of a program in the language look at the [examples](examples/) directory.
