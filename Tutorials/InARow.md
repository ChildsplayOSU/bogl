# 6) In a Row

### New Tutorial Location
These tutorials are out of date, and will be removed. The new location for all up to-date BoGL information will is [https://bogl.engr.oregonstate.edu/tutorials/](https://bogl.engr.oregonstate.edu/tutorials/).

[Previous Tutorial, Input](Input).

In this tutorial we'll talk about how to describe something closer to an actual board game. In this case, we'll talk about how to write up something that is similar to Tic Tac Toe, except with just one player.

Alright, so jumping right into it we'll create a game that's called `InARow`, and the **Board** and **Input** types will be a bit different than in our prior examples. What we're going to do is setup a game much like Tic Tac Toe, or Connect 4. In our version, we're going to end when there are 3 pieces that are in a row anywhere on our board. We'll say this counts for pieces that are in a row horizontally, vertically, or diagonally, and we'll just describe these all as being in a row. Also, we're going to say this is a 1 player game, not very challenging for that one player, but it will certainly be a bit easier for us to set this up then.

For starters, we'll be needing a 3x3 board, and to express that we can use `Array(3,3)`. Now for every space we have to think about what we want to have there. Before, we had Boards of **Int** and **Bool**, and that could work for this (for example, a space with a 1 or True has a piece on it, and 0 or False doesn't have a piece on it), but let's try something else. Both of these ways could let us describe how a space on the board is either occupied by a player or is empty, however there's another way we can go about this. First, let's write up our player type.
```
-- a type of Player, which describes the only kind of player we can have in the game
type Player = {OnlyPlayer}
```
Now this lets us describe the idea of what kinds of Players we have (only one in this case), but what about if we the case where no player is on a space? Surely we could write something like this and it would work right?
```
-- player and 'no' player
type Player = {OnlyPlayer,NoPlayer}
```
Well, this *could* work, but the problem is that `NoPlayer` *isn't really a player*. It's more like describing the absence of a player, and if our `Player` type describes that players we can have, maybe we need another way to describe what *else* we can have besides a player.

There's a handy way we can do, and we do so right in the type of our board game as follows.
```
-- a 5x5 board where each space is occupied
-- by a value of type Player `and` Empty
type Player= {OnlyPlayer}
type Space = Player & {Empty}
type Board = Array(3,3) of Space
```
This is much like what we wrote before, but what's up with the `Player & {Empty}` thing? Well, let's break it down.

First, the `Player` part seem familiar. We can have a `Player` on any space of the Board. However, this new addition of `& {Empty}` seems a bit odd. The way we could describe this, along with player, is a type of *Player and Empty*. This is akin to a union in set theory, and matches up nicely with any pre-existing knowledge you may have in this area. If this isn't too familiar that's okay, you can think of `Player & {Empty}` as taking the type of `Player`, and extending it a bit to also include the value of `Empty`. We could have modified our Player type directly to include `{OnlyPlayer,Empty}`, but again the absence of a player is *not actually a player*, so we want to keep that separate.

As a side note, we can extend by more than one value of another type, but we cannot extend by types themselves. That means something like `Int & {Bool}` is not technically correct, but `Int & {A1,A2,A3}` is.

In addition, we'll want to take a type of `Input` that is a position, since we will only be placing `OnlyPlayer` values. To take in a position, we could write something like `type Input = Int`, and run something like the `let` expressions we used in the previous [Input](Input) tutorial. But we can do a bit better than that, and since we know we want both an X and a Y, we can take them both at once like this.
```
type Input = (Int,Int)
```

This means our `Input` type is synonymous with a pair of integers (formally called a tuple of Int, Int). This lets us take inputs in the form of `(1,2)`, with the first number being the column, and the second number being the row. We should keep in mind that we have to type it in this format as well, so `1,3` or `(2 3)` *won't work*, we have to type it in the `(x,y)` format.

So, with what we have so far, we have a 5x5 board of either `OnlyPlayer` or `Empty` for each space, which makes sense for what we're trying to do.

Next, we want to setup another board equation for the board we're going to use in this game. If you're not familiar with board equations (or if you feel a bit rusty) you can go back and check on the [last tutorial about input](Input). In our game here, we'll call our Board `board`, and although these are close they are distinct (the lower-case 'b' is enough to distinguish them, otherwise BoGL wouldn't allow this!).
```
-- declare a board with all spaces being Empty at the start
board : Board
-- for all 'x' and all 'y', set (x,y) to Empty
board!(x,y) = Empty
```

Now we have a board of type `Board` (remember, the casing is important!), that is full of Empty spaces, or the absence of a Player. This produces a board that would look like the following.

```
Empty Empty Empty
Empty Empty Empty
Empty Empty Empty
```

Additionally, we can add another line to our board like so:

```
board : Board
board!(x,y) = Empty
-- for all 'x', set (x,1) to 'OnlyPlayer'
board!(x,1) = OnlyPlayer
```

This is completely valid. We can write additional 'board equations' on each line using the name of 'board' to keep changing how our board looks, so now it is:
```
OnlyPlayer OnlyPlayer OnlyPlayer
Empty      Empty      Empty
Empty      Empty      Empty
```
Each entry has been spaced out a bit to make it easier to read, but you can see every column of the first row now has a value of `OnlyPlayer`. You can play around with this, but we will want to keep all spaces as `Empty` for the rest of our example.

Now, onto the `inARow` function. This is one of many builtin functions that is already written for you, and can be used in any of your BoGL programs. This one takes 3 parameters:
1. The number of values to check as a row (such as 3 in a row, 4 in a row, etc.)
2. The value to check for (`X`, `Y`, `OnlyPlayer`, etc.)
3. The board to check


So now let's try this out in the Interpreter, if you type in: `inARow(3,OnlyPlayer,board)`, what do you think you'll get?

```
> inARow(3,OnlyPlayer,board)

True
```

Nice! But what if we try 4 in a row?
```
> inARow(4,OnlyPlayer,board)

False
```

Makes sense right? This function can come in handy when you're trying to describe board games, and in particular can be helpful when you're trying to describe a way to end a game.

To do this, we need to introduce one more idea, `while`. If you've seen other programming languages before, this may be familiar to you already, but if not don't worry, it's not too bad to describe.

`while` is a keyword in our language that means the next thing you write should be an expression that gives a value of either `True` or `False`.  

Here's a quick example of `while`, using a function we'll write called `loop`. This function takes an `Int` and returns an `Int`. The `Int` it takes is called `x`, and while that x is less than 10, 'do' x + 1, and re-evaluate the loop again with our new `Int`. In this case, the new int will be our new `x`, and we can repeat this process until `x < 10` results in false.
```
loop : Int -> Int
loop(x) = while x < 10 do x+1
```
Running this in the interpreter as `loop(1)` will give us `10`, and so will any number passed into loop that is up to 10. However, once we start passing numbers larger than 10, we will get back the exact number we pass in. This is because instead of 'doing' the `x+1`, we do nothing and return what we were given.

There is, of course, a practical limit, eventually if you type in numbers large enough you'll start getting back weird numbers, like <br>23982394829482394<br>giving us back<br>23982394829482390<br>
What's up with that?

Well, it turns out there's a practical limit to how large a number you can type into your browser, because it starts to cull off the extra part of the number it can't store. If you encounter this (or a student you are teaching), it's perfectly normal, but just don't try to compute any number *too* big, this is just a teaching language after all; and it will do it's best to represent what you give it.

Okay, so we have a general idea of while with numbers. How can we use this to play a game?

What we can do is to swap out `x < 10` for `inARow(3,OnlyPlayer,board)`, and to swap out `x+1` for some way to place pieces on the board...hmmm...and it turns out we have a very handy `Input` type of `(Int,Int)`, so we can enter in positions like `(1,1)`, or `(2,2)`.

We can us this to place pieces on our board, and since our input is of type `Player`, we can write this altogether as:
```
play : Board -> Board
play(b) = while not(inARow(3,OnlyPlayer,b)) do place(OnlyPlayer,b,input)
```

I snuck in a few new concepts, but we'll break them down here.

First the `not` function that takes a Bool, and returns it's negation, (i.e True becomes False, and vice-versa). We pass in the expression for whether we have 3 `OnlyPlayer` values in a row, which will be a Bool. This allows us to loop while we *do not have 3 in a row*.

Secondly, `place(OnlyPlayer,b,input)`, is a combination of 4 different things.
1. Another builtin function called `place`, which takes a value to place, a board to place that value on, and a position to place that value at (specicially of (Int,Int)). The result of this function is an updated board with the new piece placed at the given position.
2. `OnlyPlayer` being the sole value we will be placing at any space.
3. `b` being the board we will be placing pieces at.
4. The builtin `input` function, which if we have the type of `Input` set to `(Int,Int)` perfectly fills this last argument requirement.

Collectivelly, our resulting program should look something like this, but again it's perfectly fine if yours looks a bit different!

```
-- A game of just 'in a row'
-- once you get 3 in a row, we're done!

game InARow

type Player= {OnlyPlayer}
type Space = Player & {Empty}
type Board = Array(3,3) of Space
type Input = (Int,Int)

board : Board
board!(x,y) = Empty

play : Board -> Board
play(b) = while not(inARow(3,OnlyPlayer,b)) do place(OnlyPlayer,b,input)
```

Now playing this game can be done by running `play(board)` in the Interpreter. Upon doing this we'll see that BoGL is asking for us to enter input, and we can start putting values on our board. An example of playing this out might look like so:
```
> play(board)


 🤖 BoGL Says: Enter input, or "clear" to stop.

> (1,1)

OnlyPlayer	Empty	Empty
Empty	Empty	Empty
Empty	Empty	Empty

> (1,2)

OnlyPlayer	Empty	Empty
OnlyPlayer	Empty	Empty
Empty	Empty	Empty

> (1,3)

OnlyPlayer	Empty	Empty
OnlyPlayer	Empty	Empty
OnlyPlayer	Empty	Empty

 🤖 BoGL Says: Done reading input.

```
After we see `Done reading input.` we're all done, and our game has finished! Try out your own example to make sure it works as expected, and if it does then congratulations!

If you've gotten this far you're at the end of our tutorials! We hope that these exercises have been enlightening, and that you feel a bit better about what a programming language is about. The idea of BoGL is to give you an easy way to get started with programming. From here on out you may continue with BoGL, or you may try out a new language, but a lot of the concepts and ideas you've learned here will hopefully be helpful going forward!
