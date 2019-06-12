-- | Provides the 'unsafeDecide' function that is used to analyse a grid
-- from the perspective of the last player to place their mark on the grid.
module XO.Referee
  ( Outcome(..)
  , unsafeDecide
  )
  where


import Data.Maybe (isJust)

import XO.Grid as Grid
import XO.Mark


-- | The possible outcomes of a completed game of Tic-tac-toe.
data Outcome
  = Win
  | Squash
  deriving (Eq, Show)


-- | Assumes the given grid is valid and the given mark is the mark of the last
-- player to place their mark on the grid.
--
-- It returns @Just 'Win'@ if the player with the given mark has a winning
-- configuration on the grid.
--
-- It returns @Just 'Squash'@ if the player with the given mark didn't win and
-- there are no more available positions on the grid.
--
-- Otherwise, it returns @Nothing@ indicating that it's the other player's turn
-- to place their mark on the grid.
--
-- __N.B.:__ /'unsafeDecide' is a partial function and it is deliberately not/
-- /defined when either the grid is invalid or the given mark is not the mark/
-- /of the last player to place their mark on the grid. This is the reason why/
-- /it is given the prefix __unsafe__./
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
