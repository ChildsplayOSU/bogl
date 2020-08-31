# 4) Your Second Function

[Previous Tutorial, The Game](TheGame)

After finishing your last tutorial, you're probably wondering a little bit more about some of the aspects of this language. One of the next things we would like to go over in this tutorial is **Functions**. As we mentioned before, you can think of functions as a way of describing a process or routine that you would do. This is comparable to what you do every morning when you wake up, such as brushing your teeth, eating breakfast, going to school (but maybe not on the weekends...). These are things that you do repeatedly, and pretty much the same way each time. Similarly, you can think of functions as something that is repeated roughly the same way every time you use it.

Keeping with our focus on board games, something that can be described in a function is the idea of taking a turn. Every turn a player does something, and at the end of their turn the next player goes, and this repeats until the game ends. In this tutorial we're going to explore another simple game where we 1 turn happens, and the second player will win. Maybe not so much fun for the first player, but it'll help us grasp a bit more of Spiel.

First, let's setup a game called OneTurn, which describes what kind of game we're going to setup nicely:
```
game OneTurn
```

Again, since we don't need to worry about the board yet we can keep it simple like before:
```
type Board = Array(1,1) of Bool
```
And, we want to match our Input to what we're expecting to take in for our Board
```
type Input = Bool
```
Great! Now we also can use the basis of our type synonym from before, a.k.a the concept of different kinds of results for our game. However, we want to change it a bit, instead of just winning and losing, we want to clarify *who* wins or loses. We can do this by saying it's either player 1, or player 2.
```
type Result = {P1Wins,P2Wins}
```
Based on who won, we can also tell who lost, so we get some extra information for free here.

Now we're going to introduce another type synonym, but this one will be used to describe the idea of players in our game. We probably want to call this type synonym `Player` to make sure that lines up with what we are talking about. Now the idea of what symbols we use to describe these players is kind of up to you. In our example, we'll go with `P1` and `P2`, but you could certainly go with `A` and `B`, `X` and `O`, or something of your own design, get creative if you want!

For ours we'll write:
```
-- a type synonym called 'Player'
-- and it can be described with two
-- symbols P1 and P2, representing the 1st
-- and 2nd player in our game
type Player = {P1,P2}
```

Finally we're at the function. Like before, we'll want to determine what the *declaration* looks like. Let's say we'll call it `play` like before, but now we have to think about what it will do overall. We know that we have 2 players in our game, and 2 results, and we would like to connect these somehow. If P1 goes first, then we would like P2 to win, and the opposite if P2 goes first. This actually tells us what we would like to write in, specifically for a `Player` we can get a `Result`. We can detail that for our play function in Spiel by writing the following:
```
-- play is a function that represents
-- that this will go from a Player to a Result
play : Player -> Result
```

The next part can be a bit tricky, but the first part we can write is going to use something we call a **parameter**. A parameter (or a variable), allows us to use a letter or a word to represent a particular 'thing'. In this case, we're interested in using our parameter to represent which particular Player we're working with. This will look like `play(p) =`, where `p` is going to represent whatever player we're going to use to generate our result.

With our Player we can say something like "If p represents P1, then we want P2 to Win". We also want to say "If p represents P2, then we want to P1 to Win". To make things a little more succinct, we can combine them, and say "If p represents P1, then we want P2 to Win, *otherwise* P1 Wins". Now this is actually quite close to what we want to write in our code, so I'll put it out side-by-side with some slight adjustment so we can compare:
```
if p is the same as P1, then P2 Wins, else P1 Wins
```
And in code...
```
if p == P1 then P2Wins else P1Wins
```
So we've replaced `is the same as` with `==`, we've removed the commas, and we've combined the words P1 and P2 with Wins so that they match our symbols from before. This expresses the idea that if P1 goes first, then P2 wins, otherwise P1 wins, not so bad once you walk through it a bit!

Putting this together with the parts we wrote earlier, our complete function looks like this:
```
play : Player -> Result
play(p) = if p == P1 then P2Wins else P1Wins
```

All together our game description looks like:
```
game OneTurn

type Board = Array(1,1) of Bool
type Input = Bool
type Result = {P1Wins,P2Wins}

-- describes our players
type Player = {P1,P2}

-- describes a procedure of playing
-- a game where the player who goes second always wins
play : Player -> Result
play(p) = if p == P1 then P2Wins else P1Wins
```

And that's it! If you haven't run this already be sure to try this on the [online tool](https://bogl.engr.oregonstate.edu/) to verify everything is okay so far.

The last step is running our function, which like before is just a matter of typing out the name of the function, but now we need to add which player will go into our parameter as well. When we're calling (another way of saying to do what the function does) functions we need to use `()`, and we need to pass our parameter inside the parentheses. Calling play in our code will take one of the player symbols, so we can do:
```
play(P1)
```
Or we could do `P2` if we wanted. But whichever player we pick, we should see the other one win. If so then you've finished this tutorial! As always take a moment to go back over the concepts and think about what you've picked up so far.

[Next, we'll talk about taking input into our programs.](Input)
