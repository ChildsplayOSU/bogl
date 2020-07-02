# 2) Intro

[Previous Tutorial, Getting Started](GettingStarted)

To write your first Spiel Program. We're going to give you a few components to work with, we call these bits the **syntax**, or elements of the language. Much like the structure of this sentence that you're reading now, Spiel has a certain way of talking about things (board games that is).

For starters, the first thing we want to say is what kind of `Game` we're talking about. In this example we're talking about an Intro, so let's call is a Demo Game.
```
game DemoGame
```
We need to make sure we write the name of our game altogether without spaces, otherwise Spiel will think we're talking about something else.

And now we've declared a basic kind of game. Maybe we want to give more detail, but just so that we can remember what this means when we look back at it later. We can do this via a comment.

```
-- this is a 'Demo' game, and this is a comment
-- comments don't do anything in code,
-- but they can be helpful to describe things you want to remember later on
game Demo

{-
  You can also write comments like this,
  putting them across multiple lines.
-}
```

To be specific, every game must begin with the `game` keyword, but that doesn't include comments.

Next, every game must declare the type of the board, this looks kind of like ```type Board = Array <size> of <type>```, where <size> is a tuple of positive integers, and <type> is the type of pieces that will go on the board.

Knowing this, if we wanted to created a 10x10 board that held only Integers, we could write our game that like this
```
type Board = Array (10,10) of Int
```

Now, we also need a way to place pieces on the board. Usually, we need some sort of way to take what you type, and turn it into something that Spiel can use. We can do this by declaring what kind of data we expect to be handling as the **Input** for our program.
```
type Input = Int
```

And all together this looks like:
```
-- our 1st demo program
game DemoGame

-- declares that our boards for this game
-- will be 10x10, and will have an Integer
-- for each space
type Board = Array (10,10) of Int

-- indicates we expect Integers to be typed into
-- our program
type Input = Int
```

This is a running example, so congrats on writing your first Spiel program! If you haven't already you can try it out in our [online tool](http://bogl.engr.oregonstate.edu:5168/) to verify everything works as expected. If not take a moment to go back and double check the steps.

[Next, we'll show you how to create a simple game](TheGame).
