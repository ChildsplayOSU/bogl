# Reference Sheet for BoGL

## What it is and how to Speak it.

BoGL is a domain specific teaching language, i.e. it's a simplified programming language with an emphasis on teaching students the basics of computer science.

To this end, we've provided the following reference items for everything you can write in BoGL, and how to pronounce it. BoGL grammar will be written like
- `this`
  - followed by a definition of the syntactic element.
  - *and in italics the way 'this' be expressed in an English sentence*



## References
- `game Example`
  - Written at the top of every file. This is used to distinguish what kind of game you're writing in this program, both for your own reference and BoGL's.
  - *This program represents an 'Example' 'game'*
- `type Board = Array(<Int>,<Int>) of <AnyType>`
  - Used to define what the size of the board should be, such as `Array(5,5)` for a 5x5 board, and to denote what type of values are stored at each space, like `Array(5,5) of Int`.
  - *type Board is a 5 by 5 array of Ints*
- `type Input = <AnyType>`
  - Used to define what kind of type is allowed for input.
  - *type Input is of Int*
- `type Name = <AnyType>`
  - Used to define a type synonym, which stands for any other pre-existing type (not a new one).
  - *Name is a type synonym for Int*
  - *Name is a type synonym for A or B* (if `<AnyType>` was `{A,B}`)
- `{A,B,C,...}`
  - Represents a set of symbols that are collectively organized under a category, which we can refer to as a type.
  - *A or B or C or ...*
- ```
x : <AnyType>
x = expression
  ```
  - A value declaration, which binds a name to an equation that is computed when the program is run. We can still use the word 'function' to describe this, since an expression could be something like `input`, which we don't know until we run the program.
  - *x is a function that produces an Int, and the value of x is the result of the expression ...* (can be a literal value, like `32` or something more complex)
- ```
x : <AnyType> -> <AnyType>
x(a) = expression
```
  - A function declaration, which binds a name to an equation that takes a value of a given type, and produces a value of another type.
  - *x is a function that takes a/an `<AnyType>` represented by 'a', and computes a/an <AnyType> from the expression ...*
- ```(Int,Int)```
  - A tuple type, which is a valid type on it's own, but is composed of multiple instances of types, instead of just one.
  - *Int and an Int*
  - *Int, Int, and Int* (for a tuple of 3 Ints, as another example)
- ```
xyz : Board
xyz!(x,y) = expression -- places the value of this expr in all spaces
xyz!(1,1) = expression -- places the value of this expr in (1,1)
  -- can use one, both or a combination of more
```
  - A board declaration, which binds a name to an value of type `Board` (which is defined earlier on).
  - *xyz is a Board, where every space has the value of the expression ..., and the space 1, 1, has the value of the expression ...*
- `Int & {A1,B1}`
  - Extends the type `Int` by the set of values that are given, which are symbols. The result is a type that is valid for any Integer, A1, or B1
  - *Int, and A1 or B1*
- `xyz!(1,1)`
  - Similar to what we wrote above, this can also be used on it's own to access a space on a board, instead of setting it.
  - *Get the value of space 1, 1, from Board xyz*
- `let name = expr1 in expr2`
  - A let expression, which can be viewed as making `name = expr`, so you can write `name` in `expr2` to visually break up the example a bit. The result is the same if you wrote `expr1` directly inside `expr2`. Consider the following examples:
    - `5 + 3` == `let x = 5 in x + 3`
      - lets can use direct values
    - `1 + 2 + 3` == `let x = 1 + 2 in x + 3`
      - lets can use math
    - `1 + 2 + 3` == `let x = 1 in let y = 2 in x + y + 3`
      - lets can be nested with other expressions, including other lets
  - *let name be expression 1 in expression 2*
- `if expr1 then expr2 else expr3`
  - Used to allow switching between `expr2` and `expr3` based on the result of `expr1`, which should be something that is either True or False.
  - *if expression 1 is True then evaluate expression 2, else evaluate expression 3*
- `while expr1 do expr2`
  - Used to allow looping repeatedly over `expr2`, which should change something that ultimately makes the result of `expr1` become False. While `expr1` evaluates to True, `expr2` will be evaluated, and then `expr1` will be checked again with the updated result.
  - *while expression 1 evaluates to False, do expression 2*
