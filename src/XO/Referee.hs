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
isWin tiles mark = or $ map ((==) (m, m, m)) (arrangements tiles)
  where
    m = Just mark


isSquash :: [Tile] -> Bool
isSquash = all isJust


arrangements :: [Tile] -> [(Tile, Tile, Tile)]
arrangements [a, b, c, d, e, f, g, h, i] =
  [ (a, b, c)
  , (d, e, f)
  , (g, h, i)
  , (a, d, g)
  , (b, e, h)
  , (c, f, i)
  , (a, e, i)
  , (g, e, c)
  ]
