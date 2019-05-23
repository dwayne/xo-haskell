module XO.Grid
  ( Grid, Position, Tile
  , empty
  , set
  , isAvailable, inBounds
  , toList
  )
  where


import Data.Maybe (isNothing)

import XO.Mark (Mark)


data Grid = Grid [(Position, Mark)]

type Position = (Int, Int)


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


type Tile = (Position, Maybe Mark)


-- Let
--
--      x | o |
--     ---+---+---
-- g =    | o |
--     ---+---+---
--        |   | x
--
-- i.e. g = set (2,2) X (set (1,1) O (set (0,1) O (set (0,0) X empty)))

-- then
--
-- toList g = [ ((0, 0), Just X), ((0, 1), Just O), ((0, 2), Nothing)
--            , ((1, 0), Nothing), ((1, 1), Just O), ((1, 2), Nothing)
--            , ((2, 0), Nothing), ((2, 1), Nothing), ((2, 2), Just X)
--            ]
toList :: Grid -> [Tile]
toList (Grid moves) =
  [ (p, lookup p moves)
  | r <- [0..n]
  , c <- [0..n]
  , let p = (r, c)
  ]
  where
    n = size-1


size :: Int
size = 3
