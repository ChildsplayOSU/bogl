# Spiel-Lang

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

## Code Review Changes
| Criticism                                                                                    | Action Taken                                                                                                                                                                                                                                                                                                                                                                                                     |
|----------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Add more tutorials.                                                                          | Our informational website now includes [detailed tutorials](https://the-code-in-sheep-s-clothing.github.io/Spiel-Lang/Tutorials/All.html) which are easily accessible without having to navigate the repository itself.                                                                                                                                                                                          |
| Improve error recovery or description.                                                       | We removed the save and load buttons primarily due to a client request. To achieve this, we made our front end stateless, so it sends the full program source and the REPL expression to the back end for evaluation. This fixes the confusing UI. A user no longer has to load or save the file; instead it is automatically sent over with each REPL expression.                                               |
| The information for setting up and running the back end is not comprehensive enough.         | The primary fix for this was to host our project so that it is [web-accessible](http://access.engr.orst.edu:5168/) from an OSU network or VPN; now setting up the back end is not necessary. We will soon host the website for the general public. We also added some missing stack commands to the instructions in case someone would like to build the back end from source.                                          |
| Provide more clear set up instruction in the front end README and a Q & A for common issues. | Again, the primary fix for this was to host our project so that it is [web-accessible](http://access.engr.orst.edu:5168/ ) from an OSU network or VPN; now setting up the front end is not necessary. We will soon host the website for the general public. We also have setup instructions as well as a video walking through the setup process in case someone would like to build and run the front end from source. |
| Explain what the project is about                                                            | This has been done in our [informational website](https://the-code-in-sheep-s-clothing.github.io/Spiel-Lang/) that cleanly describes what our project is, who worked on it, why we did it, walkthrough tutorials, and details for installing the setup locally if desired.                                                                                                                                       |
