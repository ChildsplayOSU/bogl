# 5) Input

[Previous Tutorial, Your Second Function](Function).

For this tutorial we'll go over how to handle input into a program. If you've been following along with the tutorials up until this point you should have a little bit of working code from before. If you don't have your code, or you are starting for the first time, you can look back at the previous tutorials to make sure you understand everything that we're using here.

Based on what we wrote in our last tutorial, we're going to take that game and extend it a little bit. In this case we'll be using input from you (the user) to determine who will be winning this game.

First off we're going to call this `game OneTurn2`, since it's going to be our second version of our `OneTurn` game from before.

Also like before, we're okay with keeping our board simple, since we won't be using it. We can setup something like before:
```
type Board = Array(2,2) of Bool
```

Now for our input, we're going to clarify that instead of Bool like we wrote for the Board, we're going to expect Integers as input.
```
type Input = Int
```

Also, like before, we're going to declare a type synonym to represent the two players that will be participating in this game
```
type Player = {P1,P2}
```

We want to add in a type synonym for the result of our game too.
```
type Result = {P1Wins, P2Wins}
```

Next we want to define what we call a *board equation*. A board equation is a lot like the functions we saw before, but they're specific to boards. The syntax is a bit different than before as well, so we'll go over it together. First, you want to declare the name of the equation, so we'll call ours `gameBoard`.
```
gameBoard : Board
```

Now that we have the first part of our function, we need to setup the next part. For function equations, you can clarify what you want the values of the spaces on the boards to be. Since we have a board of type `Bool`, each space can have either a value of True or False.

In our game we want to start by setting all our spaces to empty. With this board we can indicate we want to do this by setting each space to False. We could also decide to set all the spaces to True as well, but the choice is up to you!

Now once we know what value we want to set all the spaces to, we write the following:
```
-- for any (x,y) on board 'gameBoard'
-- set that space to False
-- So all the spaces are False!
gameBoard!(x,y) = False
```
The syntax looks a bit odd, but let's break it down. First, `gameBoard` is the name of the board equation we're defining here. Next, the `!` is a special symbol that is used to say that this is a board equation. After that the `(x,y)` describes for *which* spaces this equation will apply to. Finally, the result of `False` is applied to each of those spaces.

If we wanted to set a specific value to a specific space, we could values instead of `(x,y)`, such as `(1,1)`.
```
-- set space 1,1 to True
gameBoard!(1,1) = True
```

Now that we setup our board, it's on to setting up the function for playing our game. Much like before, we'll call this `play`, and we'll have it take a `Board` and give us a `Result`.
```
play : Board -> Result
```

We'll call the board that is given to this function `b`, which could be the same as `gameBoard`, but we don't want to assume that (as there could be other boards!).
```
play(b) = ...
```

Now, for handling input. Now, for playing a board game, asking for input would be like saying "tell me which space you want to go to". Since we have coordinates for our board, the player can say "I would like to go to 1, 2", or "2, 3". We want to be able to handle whatever space one the board the player could give us however. The way to handle this is to rephrase the question so the player says "I would like to go to X, Y". Now, we can assume X and Y are placeholders for the actual positions we want to go to. X and Y represent what we refer to as **variables**, which are like names that describe values. Instead of having to describe how to play our game for every possible combination of (1,1) go there, (1,2) or here, (2,1) or here, (2,2) or here...we can say for a *given* X and Y, go to that space there.

Our board is only 2x2, but if we wanted to do this for a 20x20 or 100x100 board, you could see how this would take some time! Given that, variables can make our life much easier for describing what to do for any arrangement of these positions. Great, so now that we have an idea of X and Y that will represent our position, how do we write that out?

First, we have a function called `input` in Spiel we can use to read what the user types in. Since we defined the type of input earlier, we know it will be `Int` when we use it. So, conceptually what we want to do is:
```
Get Int X from input
Get Int Y from input
```
But in Spiel to read into a variable we have to use what's called a **let expression**. This is a basic way of describing that if we want a variable to be some value, we want to it to be some value for some purpose, specifically, for some *expression*. An example would be if we wanted to describe `x + 5`, we would write out:
```
-- let X be 5 in the following expression 'X + 5'
-- which will give us 10
let X = 5 in X + 5
```

Since we're reading in a value from input, we want to use that instead, and that would be like:
```
let x = input in ...
```
And that's the first part! But now we're missing the Y, so how do we work that in there. It turns out that we can write let expressions inside other let expressions. The effect is that we give X some value for an expression, where that expression is something that gives Y some value. That looks like:
```
-- let x be an input value in an expression
-- where y is also an input value
let x = input in
let y = input in ...
```
You could write this expression on 1 line, but this makes it a bit easier to read what's going on.

So now we have 2 *nested* let expressions. One on the outside that sets up X for an expression, and one on the inside that sets up Y. What we have left is the final expression that Y (and X) will be used in.

In this last expression we want to indicate that if the space at (x,y) has a value of False, then P1Wins, otherwise P2Wins! We can utilize part of the same syntax we used for our board equation before, specifically the `gameBoard!(x,y)` part. The best part is that we have a special X and Y that are from the player, and which should correspond to a space on the board. We can get the value for the space on this board, check it against True or False, and determine what the Result should be.
```
-- if the space at (x,y) is False, then P1 Wins
-- otherwise P2 Wins
if b!(x,y) == False then P1Wins else P2Wins
```

Putting the parts of this function together, we get the following:
```
-- the indenting (tab/spacing) we use is just to make things a
-- bit more readable again
play : Board -> Result
play(b) = let x = input in
          let y = input in
            if b!(x,y) == False then P1Wins else P2Wins
```

With everything we've written before, we should have:
```
game OneTurn2

type Board = Array(2,2) of Bool

type Input = Int
type Player = {P1,P2}
type Result = {P1Wins, P2Wins}

gameBoard : Board
gameBoard!(x,y) = False

play : Board -> Result
play(b) = let x = input in
          let y = input in
            if b!(x,y) == False then P1Wins else P2Wins
```

Now for playing this you can check your code in [online tool](http://bogl.uphouseworks.com:5168/) to verify everything looks correct. To play, you'll want to first type in `play(gameBoard)`, since that's the board we want to use (and `b` describes the name for any board that was passed into that function). Once you've done this you'll see a message that says you're in input mode. Now you can type in the following:
```
1
1
```
So we give 1 to the first input, and 1 again to the second input, making (x,y) equal to (1,1). Finally we should see an output of `P1Wins`, and that's it, you've successfully finished this exercise! If you want to type other things in, be sure to enter `clear` first, as this resets the game you were playing so you can start everything again. If you type something and Spiel doesn't do what you expect, try typing `clear` first to make sure everything is OK.

[Next, describing a game for items that are in a row.](InARow)
