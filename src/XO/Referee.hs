module XO.Referee
  ( Outcome(..)
  , unsafeDecide
  )
  where


import Data.Maybe (isJust)

import XO.Grid as Grid
import XO.Mark


data Outcome
  = Win
  | Squash
  deriving (Eq, Show)


unsafeDecide :: Grid -> Mark -> Maybe Outcome
unsafeDecide grid mark
  | isWin tiles mark = Just Win
  | isSquash tiles = Just Squash
  | otherwise = Nothing
  where
    tiles = Grid.toList grid


isWin :: [Tile] -> Mark -> Bool
isWin tiles mark =
  or $ zipWith (==) (currentArrangements tiles) (winningArrangements mark)


isSquash :: [Tile] -> Bool
isSquash = all isUnavailable
  where
    isUnavailable = isJust . snd


type Arrangement = (Tile, Tile, Tile)


currentArrangements :: [Tile] -> [Arrangement]
currentArrangements [a, b, c, d, e, f, g, h, i] =
  [ (a, b, c)
  , (d, e, f)
  , (g, h, i)
  , (a, d, g)
  , (b, e, h)
  , (c, f, i)
  , (a, e, i)
  , (g, e, c)
  ]


winningArrangements :: Mark -> [Arrangement]
winningArrangements mark = [((p, m), (q, m), (r, m)) | (p, q, r) <- positions]
  where
    m = Just mark


positions :: [(Position, Position, Position)]
positions =
  [ ((0, 0), (0, 1), (0, 2))
  , ((1, 0), (1, 1), (1, 2))
  , ((2, 0), (2, 1), (2, 2))
  , ((0, 0), (1, 0), (2, 0))
  , ((0, 1), (1, 1), (2, 1))
  , ((0, 2), (1, 2), (2, 2))
  , ((0, 0), (1, 1), (2, 2))
  , ((2, 0), (1, 1), (0, 2))
  ]
