# Spiel-Lang

## Spiel is an implementation of an interpreter for the BoGL teaching language intended to model the domain of board game specification for use in middle school computer science and software engineering education

Please see our website at [https://the-code-in-sheep-s-clothing.github.io/Spiel-Lang/](https://the-code-in-sheep-s-clothing.github.io/Spiel-Lang/) for up to date details on the language, how to get started with it, tutorials, and more.

This is the back end of the implementation which includes the interpreter itself and a server. To actually use the language you need the [front end](https://github.com/The-Code-In-Sheep-s-Clothing/Spiel-Front) which includes an editor and REPL. 

## Installation
1. Install [the haskell tool stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

   * *optional:* Install [doctest](https://hackage.haskell.org/package/doctest) for haskell to run the test suite
   * These tests can be run with `stack test`
   
   This will put a spielserver executable in your path which the front end can then use. 
   
## Examples 
To see an example of a program in the language look at the [examples](examples/) directory.

## Code Review Summary
- Students really wanted tutorials that were easy to find and follow.
- Installation was a little tough (but that shouldn't be an issue with our web setup now
- Some people didn't know what our project was, so some sort of info page would be helpful (I talked with Alex about this, a github pages website might be a nice landing page to describe what this is, and maybe some description around our tutorials too, and I started setting something up in this direction). I figured something really simple where they can read a sentence to see what this is, and read a paragraph to find out more about that, and if they still want more we can show them the tuts.

Based on this feedback we have implemented a github pages website (available at the link above) that cleanly describes what our project is, who worked on it, why we did it, walkthrough tutorials and details for installing the setup locally if desired. However, for this we have setup our tool online at [http://access.engr.orst.edu:5168/](http://access.engr.orst.edu:5168/) (only accessible from OSU's network from on campus or via the VPN). We think that both of these changes should help by making installation a non-issue, making user accessibility a priority, and providing a better presentation to guide a user through our project.
