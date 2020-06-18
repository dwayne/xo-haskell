# xo

A Haskell library and CLI game for Tic-tac-toe.

## The CLI game

Here's how to play it:

```
> xo
Welcome to Tic-tac-toe
Play as many games as you want
Press Ctrl-C to exit at any time

Your turn (X)
   |   |
---+---+---
   |   |
---+---+---
   |   |
> 2 2
The computer played at 1, 3
Your turn (X)
   |   | O
---+---+---
   | X |
---+---+---
   |   |
> 1 1
The computer played at 3, 3
Your turn (X)
 X |   | O
---+---+---
   | X |
---+---+---
   |   | O
> ...
```

By default `X` plays first and is controlled by a human player (you) whereas `O`
plays second and is controller by the computer.

Here are some other ways to run the game:

```
> xo --first o
# x is human, o is computer and o plays first

> xo -x computer -o human
# x is computer, o is human and x plays first

> xo -x computer -o human --first o
# x is computer, o is human and o plays first

> xo -x computer
# computer vs computer, plays for 25 rounds by default

> xo -x computer --rounds 50
# computer vs computer, plays for 50 rounds
```

Use `xo --help` to learn more.

## Design overview

The game is decomposed into a [library](src) and an [executable](cli).

The library consists of 5 modules:

1. [XO.Mark](src/XO/Mark.hs) contains the X's and O's a player uses to mark an
   available tile.
2. [XO.Grid](src/XO/Grid.hs) provides a 3x3 grid that can contain either marked
   or unmarked tiles.
3. [XO.Game](src/XO/Game.hs) exposes an API that is used to enforce the game
   logic for Tic-tac-toe.
4. [XO.AI](src/XO/AI.hs) implements a minimax algorithm to determine the best
   positions to place a mark.
5. [XO.Referee](src/XO/Referee.hs) exports a function that is used to decide
   when a game is over and who won or squashed the game. `XO.Game` and `XO.AI`
   are intended to be the only users of the referee.

The executable depends on the library and uses it to implement the command-line
interface.

- [XO.CLI.Player](cli/XO/CLI/Player.hs) provides a data type to tag which mark
  is controlled by a human and which by the computer.
- [XO.CLI.Orchestrator](cli/XO/CLI/Orchestrator.hs) controls the UI and flow
  of the game based on the players involved: "human vs computer",
  "human vs human" or "computer vs computer". "computer vs computer" makes use
  of the
  [non-interactive orchestrator](cli/XO/CLI/Orchestrator/Noninteractive.hs)
  whereas when a human player is involved the
  [interactive orchestrator](cli/XO/CLI/Orchestrator/Interactive.hs) is used.
- [XO.CLI.Options](cli/XO/CLI/Options.hs) handles the command-line options.
- [Main](cli/Main.hs) brings it all together.

## Library usage examples

**XO.Mark**

```haskell
>>> import XO.Mark as Mark
>>> X
X
>>> O
O
>>> Mark.swap X
O
>>> Mark.swap O
X
>>> :t X
X :: Mark
```

**XO.Grid**

```haskell
>>> import XO.Grid as Grid
>>> Grid.empty
.........
>>> Grid.set (0, 0) X Grid.empty
X........
>>> Grid.set (2, 2) O (Grid.set (0, 0) X Grid.empty)
X.......O

>>> Grid.inBounds (2, 3)
False

>>> let grid = Grid.set (2, 2) O (Grid.set (0, 0) X Grid.empty)

>>> Grid.isAvailable (0, 0) grid
False
>>> Grid.isAvailable (1, 1) grid
True

>>> Grid.availablePositions grid
[(0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1)]

>>> Grid.toList grid
[Just X, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just O]
```

Notice that the grid doesn't care about Tic-tac-toe's game logic so it's
possible to do the following:

```haskell
>>> let badGrid = Grid.set (0, 1) X (Grid.set (1, 1) X Grid.empty)
```

`XO.Game` is used to enforce the game logic.

**XO.Game**

```haskell
>>> import XO.Game as Game

>>> let game0 = Game.new X
>>> game0
{ grid = ........., turn = X }
-- The grid is empty and it's X's turn to play

>>> let Right game1 = Game.play (0, 0) game0
>>> game1
{ grid = X........, turn = O, lastPosition = (0,0) }
-- X was marked at (0, 0) and it's now O's turn to play

>>> let Right game2 = Game.play (2, 2) game1
>>> game2
{ grid = X.......O, turn = X, lastPosition = (2,2) }
-- O was marked at (2, 2) and it's now X's turn to play and so on ...

>>> Game.play (0, 0) game2
Left Unavailable

>>> Game.play (2, 3) game2
Left OutOfBounds

>>> Game.grid game2
X.......O
>>> Game.turn game2
X
>>> Game.outcome game2
Nothing
```

If you explore the implementation of [XO.Game](src/XO/Game.hs) you'd notice
that it's impossible to violate the rules of Tic-tac-toe.

**XO.AI**

```haskell
>>> import XO.AI as AI

>>> AI.getPositions game2
[(0, 2), (2, 0)]
-- If X is marked at one of these positions then with perfect play X can win

>>> let Right game3 = Game.play (0, 2) game2

>>> let Right game4 = Game.play (0, 1) game3

>>> AI.getPositions game4
[(2, 0)]

>>> let Right game5 = Game.play (2, 0) game4

--    0   1   2
-- 0  X | O | X
--   ---+---+---
-- 1    |   |
--   ---+---+---
-- 2  X |   | O
--
-- You see, X's win is inevitable now

>>> let Right game6 = Game.play (1, 0) game5
>>> AI.getPositions game6
[(1, 1)]

>>> let Right game7 = Game.play (1, 1) game6
-- X won

>>> Game.grid game7
XOXOX.X.O
>>> Game.turn game7
X
>>> Game.outcome game7
Just Win
```

Since `XO.Referee` is intended to be used only by `XO.Game` and `XO.AI` I won't
illustrate its usage here. However, I invite you to check out its
[implementation](src/XO/Referee.hs).

Each library module has extensive unit test coverage which can be found
[here](tests/Test/XO). If you need more examples of how the library is intended
to be used then you will find it instructive to read the tests.

## More on the executable

Execution starts with the `main` function in the `Main` module. The
command-line options are parsed into the `XO.CLI.Options.Options` data type
which is then passed to the `XO.CLI.Orchestrator.run` function. If a human is
involved in the game play then the `XO.CLI.Orchestrator.Interative.run`
function is called with the appropriate arguments. Otherwise, the computer is
playing against itself and the `XO.CLI.Orchestrator.Noninteractive.run`
function is called with the appropriate arguments.

Read the source code of the
[XO.CLI.Orchestrator.Interative](cli/XO/CLI/Orchestrator/Interactive.hs) and
[XO.CLI.Orchestrator.Noninterative](cli/XO/CLI/Orchestrator/Noninteractive.hs)
modules to fully understand how the game play is managed.
