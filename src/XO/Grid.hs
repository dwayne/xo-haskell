-- | A 3x3 Tic-tac-toe grid which can contain either spaces or 'XO.Mark.Mark's.
--
-- It is important to note that this API does not restrict the use of the grid
-- to only valid grids, i.e. it is possible to construct grids you will never
-- reach during game play. A valid grid is one that can be reached by following
-- the rules of Tic-tac-toe.
--
-- See "XO.Game" for an API that enforces the rules of Tic-tac-toe and
-- maintains a valid grid.
module XO.Grid
  ( Grid, Position

  -- * Create
  , empty

  -- * Modify
  , set

  -- * Query
  , isAvailable, inBounds
  , availablePositions

  -- * Convert
  , Tile
  , toList
  )
  where


import Data.Maybe (isNothing, maybe)
import XO.Mark (Mark)


-- | A 3x3 Tic-tac-toe grid.
data Grid = Grid [(Position, Mark)]

-- | The first value is the 0-based @row@ and the second value is the 0-based
-- @column@.
type Position = (Int, Int)


-- | Two grids are equal if and only if they are equal at each position.
instance Eq Grid where
  grid1 == grid2 = toList grid1 == toList grid2


-- | >>> show empty
-- "........."
--
-- >>> show (set (1,1) O (set (0,0) X empty))
-- "x...o...."
instance Show Grid where
  show grid = map showTile (toList grid)
    where
      showTile tile = head (maybe "." show tile)


-- | An empty grid.
empty :: Grid
empty = Grid []


-- | Returns a new grid that contains the mark at the position.
set :: Position -> Mark -> Grid -> Grid
set p mark (Grid moves) = Grid ((p, mark):moves)


-- | Returns 'True' if and only if the grid does not contain a mark at the
-- position.
isAvailable :: Position -> Grid -> Bool
isAvailable p (Grid moves) = p `notIn` moves


-- | Let the position be @(r, c)@. It returns 'True' if and only if
-- @0 <= r <= 2@ and @0 <= c <= 2@.
inBounds :: Position -> Bool
inBounds (r, c) = r >= 0 && r < size && c >= 0 && c < size


-- | Returns all unmarked positions in
-- <https://en.wikipedia.org/wiki/Row-_and_column-major_order row-major order>.
availablePositions :: Grid -> [Position]
availablePositions (Grid moves) =
  [p | r <- [0..n], c <- [0..n], let p = (r, c), p `notIn` moves]


-- | A space or a 'XO.Mark.Mark'.
type Tile = Maybe Mark


-- | Returns a list of 'XO.Grid.Tile's in
-- <https://en.wikipedia.org/wiki/Row-_and_column-major_order row-major order>.
--
-- >>> toList (set (1,1) O (set (0,0) X empty))
-- [Just x, Nothing, Nothing, Nothing, Just o, Nothing, Nothing, Nothing, Nothing]
toList :: Grid -> [Tile]
toList (Grid moves) = [lookup (r, c) moves | r <- [0..n], c <- [0..n]]


-- Helpers


notIn :: Eq a => a -> [(a, b)] -> Bool
key `notIn` assocs =
  isNothing (lookup key assocs)


size :: Int
size = 3


n :: Int
n = size-1
