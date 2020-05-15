# 5) Input

[Previous Tutorial, Your Second Function](Function).

For this tutorial we'll go over how to handle input into a program. If you've been following along with the tutorials up until this point you should have a little bit of working code from before. If you don't have your code, or you are starting for the first time, you can look back at the previous tutorials to make sure you understand everything that we're using here.

Based on what we wrote in our last tutorial, we're going to take that game and extend it a little bit. In this case we'll be using input from you (the user) to determine who will be winning this game.

First off we're going to call this `game OneTurn2`, since it's going to be our second version of our `OneTurn` game from before.

Also like before, we're okay with keeping our board simple, since we won't be using it. We can setup something like before:
```
type Board = Array(1,1) of Bool
```

Now for our input, we're going to clarify that instead of Bool like we wrote for the Board, we're going to expect Integers as input.
```
type Input = Int
```

[Next, describing a game for items that are in a row.](InARow)
