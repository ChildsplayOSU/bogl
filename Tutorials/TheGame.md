# 3) The (Simple) Game

### New Tutorial Location
These tutorials are out of date, and will be removed. The new location for all up to-date BoGL information will is [https://bogl.engr.oregonstate.edu/tutorials/](https://bogl.engr.oregonstate.edu/tutorials/).

[Previous Tutorial, Intro](Intro)

In this tutorial, we will walk through how to create a basic game that end as soon as it starts, called 'The Game'. This will demonstrate that we can describe a game even if it doesn't do very much. If you haven't already, we would highly recommend you go over the tutorials before this one, so you will be familiar with some of the concepts we'll be using.

First, we'll create a new game called TheGame
```
game TheGame
```

Even though we don't plan on doing much in this game, we still are required to define the **Board** and **Input** types for this game. First, we'll define a simple 1x1 board (a single space), but this time we'll make it store Bool!

Bool may seem unfamiliar at first, at it's an odd name when you think about it. However, it's used to represent the basic of concept of something you are quite familiar with (hopefully!), True and False. That's it! If we're talking about a Bool (or a Boolean) we're talking about something that can be either True or False. So, this board can have only 1 of 2 possible values in this one space, a value of True, or a value of False.
```
type Board = Array(1,1) of Bool
```

Now, if you're thinking ahead, you might wondering how we will be able to read something that will be a **Bool** that we can use on the board. Although we won't be reading anything just yet, BoGL requires us to declare what type of **Input** we will be expecting. In this case, we can simply say it should match what we have on our board, a Bool.
```
type Input = Bool
```

Now, we've been working with a few types so far, Bool, Int, and Board. These all are ways of allowing us to represent things in BoGL. Int for positive whole numbers, Bool for True and False, and a Board to represent what size of game board we're working with and what we can 'put' on it.

However, we can introduce what we call *type synonyms* as well. These are used to help us describe things we want that can categorized, such as to describe the idea of a **Result** in a game. We know there is such a thing as **Winning** and **Losing**, so is there a way to express this in BoGL without using Bool, Int, or something else? It turns out there is, and we can do just that like this:
```
-- type synonym called Result
-- describes the notions of winning as 'YouWin' and losing as 'YouLose'
type Result = {YouWin,YouLose}
```
And there we go! Just like that we've used what are called **Symbols** to express what the ideas of winning and losing are. We have a symbol to represent each idea of what a **Result** could be. If we wanted, we could even add another symbol for a tie, like so:
```
-- adds a symbol to describe a result
-- that is a tie
type Result = {YouWin,YouLose,ItsATie}
```
Notice that just like the name of our game, BoGL likes the things we are describing to be all in one word.

With what we have written out so far, we can write a way to play this game. Since playing is an action, we can describe it as a routine or a procedure, i.e. something that we do. In Computer Science, we commonly refer to this as a *function*, which is very much a routine or procedure that does something. For our game, we want to define a function for playing our game, and since this is a simple game we're going to say the result is we win every time.

To start writing this we need to write a **declaration** for our function, something that states the name and what it returns. It also can say what it takes, but we'll save that for the next lesson. Our function will be called `play`, and it will return something that should be a `Result`, so the 1st part will look like:
```
play : Result
```
Great, now that we've *declared* what it's called, and what it does (gives us a result), we need to *define* how it does this. We can do this by writing in what we call a **definition**. In this case, our function can do this by simply winning, so we can say that playing is essentially just winning this game; and we can say that like:
```
play = YouWin
```

And that's our play function, together it should look like:
```
play : Result
play = YouWin
```

And now, with the rest of our code, we have:
```
-- our simple game that we always win
game TheGame

-- has a 1x1 board that has either True or False
type Board = Array(1,1) of Bool

-- and we're expecting True or False from the user
type Input = Bool

-- describes the idea of what results our game can have
type Result = {YouWin,YouLose,ItsATie}

-- and finally, a function that describes the procedure/routine for
-- playing this game
play : Result
play = YouWin
```

You can type this code into the editor, run it by typing `play`, and you should get the result back of 'YouWin'. If so, congrats, you have completed this tutorial! Take a moment to think about what we've gone over here.

When you're ready, [our next tutorial is all about writing functions](Function).
