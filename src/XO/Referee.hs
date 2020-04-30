-- | It exports the 'unsafeDecide' function that is used to analyse a grid to
-- determine the outcome of a game.
module XO.Referee
  ( Outcome(Win, Squash)
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


-- | Assumes the grid is valid and that mark is the mark on the last tile to be
-- marked.
--
-- It returns @Just 'Win'@ if the marked tile resulted in a winning row, column
-- or diagonal.
--
-- It returns @Just 'Squash'@ if the marked tile didn't result in a winning row,
-- column or diagonal and there are no more available positions.
--
-- Otherwise, it returns @Nothing@ indicating that it's the other player's turn
-- to mark a tile.
--
-- __N.B.:__ /'unsafeDecide' is a partial function and it is deliberately not/
-- /defined when either the grid is invalid or the given mark is not the mark/
-- /of the last tile to be marked. This is the reason why it is given the/
-- /prefix __unsafe__./
unsafeDecide :: Grid -> Mark -> Maybe Outcome
unsafeDecide grid mark
  | isWin tiles mark = Just Win
  | isSquash tiles = Just Squash
  | otherwise = Nothing
  where
    tiles = Grid.toList grid


isWin :: [Tile] -> Mark -> Bool
isWin tiles mark = or $ map ((==) (t, t, t)) (arrangements tiles)
  where
    t = Just mark


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
