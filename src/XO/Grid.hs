module XO.Grid
  ( Grid, Position, Tile
  , empty
  , set
  , isAvailable, inBounds
  , availablePositions
  , toList
  )
  where


import Data.Maybe (isNothing)

import XO.Mark (Mark)


data Grid = Grid [(Position, Mark)]

type Position = (Int, Int)

type Tile = Maybe Mark


empty :: Grid
empty = Grid []


set :: Position -> Mark -> Grid -> Grid
set p mark (Grid moves) =
  Grid ((p, mark):moves)


isAvailable :: Position -> Grid -> Bool
isAvailable p (Grid moves) =
  isNothing (lookup p moves)


inBounds :: Position -> Bool
inBounds (r, c) =
  r >= 0 && r < size && c >= 0 && c < size


availablePositions :: Grid -> [Position]
availablePositions (Grid moves) =
  [(r, c) | r <- [0..n], c <- [0..n], isNothing (lookup (r, c) moves)]
  where
    n = size-1


toList :: Grid -> [Tile]
toList (Grid moves) = [lookup (r, c) moves | r <- [0..n], c <- [0..n]]
  where
    n = size-1


size :: Int
size = 3
