# 6) In a Row

[Previous Tutorial, Input](Input).

In this tutorial we'll talk about how to describe something closer to an actual board game. In this case, we'll talk about how to write up something that is similar to Tic Tac Toe, except with just one player.

Alright, so jumping right into it we'll create a game that's called `InARow`, and the **Board** and **Input** types will be a bit different than in our prior examples. What we're going to do is setup a game much like Tic Tac Toe, or Connect 4. In our version, we're going to end when there are 3 pieces that are in a row anywhere on our board. We'll say this counts for pieces that are in a row horizontally, vertically, or diagonally, and we'll just describe these all as being in a row. Also, we're going to say this is a 1 player game, not very challenging for that one player, but it will certainly be a bit easier for us to set this up then.

For starters, we'll be needing a 5x5 board, and to express that we can use `Array(5,5)`. Now for every space we have to think about what we want to have there. Before, we had Boards of **Int** and **Bool**, and that could work for this (for example, a space with a 1 or True has a piece on it, and 0 or False doesn't have a piece on it), but let's try something else. Both these ways could let us describe how a space on the board is either occupied by a player or is empty, however we can do this a bit more directly with the following.
```
-- a 5x5 board where space is occupied
-- by the only player or is empty
type Board = Array(5,5) of {OnlyPlayer,Empty}
```
This is much like what we wrote before, but what's up with the `{OnlyPlayer,Empty}` thing? Well let's break it down.

First, the `{}` braces are used when we have more than one type that can be on the board. One of these is given to us by BoGL, `Empty`, we like to say this is a *built-in* part of the language, so you don't have to write it up yourself. You can think of Empty much like how we used the symbols from the other tutorials, in the sense that this is just another way of representing the idea of 'nothing'. The other part, `OnlyPlayer`, is something that is not given to us however. We need to write this up ourselves, and we'll do that  right here.
```
type Player = {OnlyPlayer}
```

Once we have our OnlyPlayer setup, we'll have a 5x5 board of either `OnlyPlayer` pieces or `Empty` spaces, which makes sense for what we're trying to do.

Next, we want to setup another board equation for the board we're going to use in this game. If you're not familiar with board equations (or if you feel a bit rusty) you can go back and check on the [last tutorial about input](Input). In our game here, we'll call our Board `board`, and although these are close they are distinct (the lower-case 'b' is enough to distinguish them, otherwise BoGL wouldn't allow this!).
```
-- declare a board with all spaces being Empty at the start
board : Board
board!(x,y) = Empty
```

....loop code continued from here!....

Like before we need to clarify what kind of Input we're expecting from the user. When we're playing on this board we want to be able to say where we are planning on putting pieces down. In this regard, it would be nice if we could clarify X and Y coordinates on the board to place our pieces, with X being horizontal and Y being vertical. In doing so we could say place a piece at `(1,1)`, but we also would need to describe where we're starting from on the X and Y axis. In BoGL, the board will appear to always start at the top left corner, and the X axis increases to the right, while the Y axis increases downwards. Just keep this in mind, as it can be easy to think we're starting in the bottom left corner and moving up and right instead.

...Rest of this Tutorial Coming Soon...

[Next, Counting Pieces of Board](BoardCount).
