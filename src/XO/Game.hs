-- | The game logic for Tic-tac-toe.
--
-- The rules of Tic-tac-toe are enforced and a valid grid is maintained.
--
-- To create a new game call the 'new' function.
--
-- >>> let game0 = new X
--
-- This creates a new game where the first player uses mark 'X'.
--
-- To play a game you call the 'play' function with the position you want to
-- mark.
--
-- >>> let Right game1 = play (1, 1) game0
--
-- Notice that you don't pass the mark since that's tracked internally by the
-- game, i.e. it knows whose turn it is.
--
-- Hence, @game1@ represents a game where the player using mark 'X' has played
-- at (1, 1) and the player using mark 'O' has the next play.
--
-- If we now call 'play' with @game1@ and a position,
--
-- >>> let Right game2 = play (0, 2) game1
--
-- then @game2@ would represent a game where the player using mark 'O' has now
-- played at (0, 2) and the player using mark 'X' has the next play.
--
-- @
--    0   1   2
-- 0    |   | O
--   ---+---+---
-- 1    | X |
--   ---+---+---
-- 2    |   |
-- @
--
-- History:
--
--   * X played at (1, 1)
--   * O played at (0, 2)
--   * X to play
--
-- Game play continues until either one of the players wins or the game is
-- squashed. You can determine whether or not a game is over by querying the
-- outcome of the game using the 'outcome' function.
--
-- >>> outcome game2
-- Nothing
--
-- An outcome of @Nothing@ means the game is not over. See 'outcome' for further
-- details.
--
-- When a game is over and you want to continue playing then it's recommended
-- to use the 'renew' function if you want the following:
--
--   * the winning player gets to play first in the next game, or
--   * if the game was squashed the player that didn't play first in the
--     completed game gets to play first in the next game,
--     i.e. the first player alternates.
module XO.Game
  ( Game

  -- * Create
  , new

  -- * Game play
  , play, Error(OutOfBounds, Unavailable)
  , renew

  -- * Query
  , availablePositions
  , grid, turn, lastPosition
  , Outcome(Win, Squash), outcome
  )
  where


import qualified Data.List as List

import qualified XO.Grid as Grid
import XO.Grid (Grid, Position)
import XO.Mark as Mark
import XO.Referee as Referee


-- | An abstract data type for tracking a game of Tic-tac-toe. The API built
-- for it enforces the game logic of Tic-tac-toe.
data Game
  = Start Grid Mark
  | Play Grid Mark Position
  | GameOver Grid Mark Position Outcome


-- | let game0 = new X
--
-- >>> show game0
-- "{ grid = ........., turn = X }"
--
-- let Right game1 = play (1, 1) game0
--
-- >>> show game1
-- "{ grid = X........, turn = O, lastPosition = (1,1) }"
instance Show Game where
  show (Start grid turn) =
    showInBrackets [showGrid grid, showTurn turn]
  show (Play grid turn p) =
    showInBrackets [showGrid grid, showTurn turn, showLastPosition p]
  show (GameOver grid turn p o) =
    showInBrackets
      [ showGrid grid
      , showTurn turn
      , showLastPosition p
      , showOutcome o
      ]


showGrid :: Grid -> String
showGrid grid = "grid = " ++ show grid


showTurn :: Mark -> String
showTurn turn = "turn = " ++ show turn


showLastPosition :: Position -> String
showLastPosition p = "lastPosition = " ++ show p


showOutcome :: Outcome -> String
showOutcome o = "outcome = " ++ show o


showInBrackets :: [String] -> String
showInBrackets ss = "{ " ++ List.intercalate ", " ss ++ " }"


-- | Starts a new game of Tic-tac-toe such that the first player uses mark.
--
-- For e.g.
--
-- >>> let game = new O
--
-- The grid managed by @game@ will be empty and the first player will use the
-- mark @O@.
new :: Mark -> Game
new = Start Grid.empty


-- | Suppose it's @X@'s turn. Then, 'play' attempts to mark @X@ on the tile at
-- the given position.
--
-- 1. If the position is not within the boundaries of the grid then
--    @Left 'OutOfBounds'@ is returned.
--
-- 2. If the tile at the position is already marked then
--    @Left 'Unavailable'@ is returned.
--
-- 3. Otherwise the tile at the position is marked and an updated game is
--    returned.
play :: Position -> Game -> Either Error Game
play p (Start grid mark)  = checkedPlay p mark grid
play p (Play grid mark _) = checkedPlay p mark grid
play _ game               = Right game


checkedPlay :: Position -> Mark -> Grid -> Either Error Game
checkedPlay p mark grid =
  if Grid.inBounds p then
    if Grid.isAvailable p grid then
      Right (uncheckedPlay p mark grid)
    else
      Left Unavailable
  else
    Left OutOfBounds


uncheckedPlay :: Position -> Mark -> Grid -> Game
uncheckedPlay p mark grid =
  let
    nextGrid = Grid.set p mark grid
  in
    case Referee.unsafeDecide nextGrid mark of
      Nothing ->
        Play nextGrid (Mark.swap mark) p

      Just outcome ->
        GameOver nextGrid mark p outcome


-- | The possible errors that can occur when attempting to play the game.
data Error
  = OutOfBounds
  | Unavailable
  deriving (Eq, Show)


-- | Starts a new game.
--
-- If the game is squashed then the first player of the new game is changed,
-- otherwise the first player of the new game is whoever had the next play or
-- won in the completed game.
renew :: Game -> Game
renew (Start _ m)             = new m
renew (Play _ m _)            = new m
renew (GameOver _ m _ Win)    = new m
renew (GameOver _ m _ Squash) = new (Mark.swap m)


-- | When the game is in-progress it returns the positions of the unmarked
-- tiles. However, when the game is over no play is possible and so no
-- positions are returned.
availablePositions :: Game -> [Position]
availablePositions (Start grid _)  = Grid.availablePositions grid
availablePositions (Play grid _ _) = Grid.availablePositions grid
availablePositions _               = []


-- | Returns the grid that's managed by the game.
--
-- The returned grid is always valid.
grid :: Game -> Grid
grid (Start g _)        = g
grid (Play g _ _)       = g
grid (GameOver g _ _ _) = g


-- | The returned mark is to be interpreted in one of three ways:
--
--   * If the game is not over then the returned mark represents the mark of
--     the player to play next.
--   * If the game is over and one of the players won then the returned mark
--     represents the mark of the winning player.
--   * If the game is over and squashed then the returned mark represents the
--     mark of the player that squashed the game.
turn :: Game -> Mark
turn (Start _ m)        = m
turn (Play _ m _)       = m
turn (GameOver _ m _ _) = m


-- | Returns the position of the last tile to be marked, if any.
lastPosition :: Game -> Maybe Position
lastPosition (Start _ _)        = Nothing
lastPosition (Play _ _ p)       = Just p
lastPosition (GameOver _ _ p _) = Just p


-- | Returns the outcome of the game.
--
-- It will be one of @Nothing@ (which means the game is not over), @Just 'Win'@
-- or @Just 'Squash'@.
outcome :: Game -> Maybe Outcome
outcome (GameOver _ _ _ o) = Just o
outcome _                  = Nothing
