# BoGL

[![Build Status](https://api.travis-ci.com/The-Code-In-Sheep-s-Clothing/bogl.svg?branch=master)](https://travis-ci.com/github/The-Code-In-Sheep-s-Clothing/bogl)

## An implementation of the BoGL programming language

## BoGL is a teaching language intended to model the domain of board game specification. It is used in early computer science and software engineering education.

You can learn more about the language on our [tutorial site](https://bogl.engr.oregonstate.edu/tutorials/). To write and run a BoGL program, you can use our [REPL](https://bogl.engr.oregonstate.edu/).

If you are developing on any part of the BoGL stack, you can read our [technical documentation](https://bogl.engr.oregonstate.edu/technical/).

This is the back end of the implementation which includes the interpreter itself and a server. To actually use the language you need the [front end](https://github.com/The-Code-In-Sheep-s-Clothing/bogl-editor) which includes an editor and REPL.

## Installation
1. Clone this repository
2. Install [the haskell tool stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
3. Inside the project repository run `stack setup`
4. Run `stack install`. This will put a boglserver executable in your path which the front end can then use.
   * *optional:* Run our test suite with `stack test`
