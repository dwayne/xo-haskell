-- | The computer's \"brain\".
--
-- It exports the 'getPositions' function which uses a
-- <https://en.wikipedia.org/wiki/Minimax minimax> algorithm to determine the
-- positions of the best tiles to mark based on the state of a game.
module XO.AI (getPositions) where


import Data.Maybe (maybeToList)


import qualified XO.Game as Game
import XO.Game (Game)
import XO.Grid as Grid
import XO.Mark as Mark
import XO.Referee as Referee


-- | Returns the positions of the best tiles to mark based on the state of the
-- game.
--
-- In general, it uses a <https://en.wikipedia.org/wiki/Minimax minimax>
-- algorithm. However, there are three cases where minimax is not needed:
--
-- 1. When the game is over. There are no available positions and so the empty
--    list has to be returned.
--
-- 2. When only one position is available, that position must be returned.
--
-- 3. When every position is available, every position can be returned.
getPositions :: Game -> [Position]
getPositions game
  | n == 0 || n == 1 || n == 9 = availablePositions
  | otherwise                  = positions $ minimax game
  where
    n = length availablePositions
    availablePositions = Game.availablePositions game


-- Minimax


minimax :: Game -> Value
minimax = maximize 0 . gameTree


type Depth = Int


maximize :: Depth -> Tree Game -> Value
maximize depth (Node game []) = (minScore game, depth, [])
maximize depth (Node _ subtrees) =
  foldl1 maxSum $ map (evaluate minimize depth) subtrees


minimize :: Depth -> Tree Game -> Value
minimize depth (Node game []) = (maxScore game, depth, [])
minimize depth (Node _ subtrees) =
  foldl1 minSum $ map (evaluate maximize depth) subtrees


evaluate :: (Depth -> Tree Game -> Value) -> Depth -> Tree Game -> Value
evaluate f depth subtree@(Node game _) =
  updatePositions (lastPosition game) (f (depth+1) subtree)
  where
    lastPosition = maybeToList . Game.lastPosition


-- Game Tree


gameTree :: Game -> Tree Game
gameTree = iterateTree moves


moves :: Game -> [Game]
moves game =
  map (fromRight . flip Game.play game) (Game.availablePositions game)
  where
    fromRight (Right r) = r


data Tree a = Node a [Tree a]


iterateTree :: (a -> [a]) -> a -> Tree a
iterateTree f x = Node x (map (iterateTree f) (f x))


-- Score


type Score = Int


maxScore :: Game -> Score
maxScore game =
  case Game.outcome game of
    Nothing ->
      0

    Just Squash ->
      1

    Just Win ->
      2


minScore :: Game -> Score
minScore = negate . maxScore


-- Value


type Value = (Score, Depth, [Position])


positions :: Value -> [Position]
positions (_, _, ps) = ps


updatePositions :: [Position] -> Value -> Value
updatePositions ps (s, d, _) = (s, d, ps)


minSum :: Value -> Value -> Value
minSum v1@(s1, d1, ps1) v2@(s2, d2, ps2)
  | s1 < s2   = v1
  | s2 < s1   = v2
  | d1 < d2   = v1
  | d2 < d1   = v2
  | otherwise = (s1, d1, ps1 ++ ps2)


maxSum :: Value -> Value -> Value
maxSum v1@(s1, d1, ps1) v2@(s2, d2, ps2)
  | s1 > s2   = v1
  | s2 > s1   = v2
  | d1 < d2   = v1
  | d2 < d1   = v2
  | otherwise = (s1, d1, ps1 ++ ps2)
