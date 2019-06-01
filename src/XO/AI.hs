module XO.AI (getPositions) where


import XO.Game as Game
import XO.Grid as Grid
import XO.Mark as Mark
import XO.Referee as Referee


type Score = Int
type Depth = Int
type Result = (Score, Depth, [Position])


getPositions :: Game -> [Position]
getPositions game
  | n == 9 = availablePositions
  | n == 1 = availablePositions
  | otherwise = positions (maximize max min 0 grid)
  where
    n = length availablePositions
    positions (_, _, ps) = ps

    max = Game.turn game
    min = Mark.swap max

    availablePositions = Grid.availablePositions grid
    grid = Game.grid game


maximize :: Mark -> Mark -> Depth -> Grid -> Result
maximize max min depth grid =
  case Referee.unsafeDecide grid min of
    Nothing ->
      let
        positions = Grid.availablePositions grid
        set p = Grid.set p max grid
        nextGrids = map set positions
        mins = map (minimize min max (depth+1)) nextGrids
        combine ((s, d, _), p) = (s, d, [p])
        nextResults = map combine (zip mins positions)
      in
        foldr1 maxResult nextResults

    Just outcome ->
      (minScore outcome, depth, [])


minimize :: Mark -> Mark -> Depth -> Grid -> Result
minimize min max depth grid =
  case Referee.unsafeDecide grid max of
    Nothing ->
      let
        positions = Grid.availablePositions grid
        set p = Grid.set p min grid
        nextGrids = map set positions
        maxs = map (maximize max min (depth+1)) nextGrids
        combine ((s, d, _), p) = (s, d, [p])
        nextResults = map combine (zip maxs positions)
      in
        foldr1 minResult nextResults

    Just outcome ->
      (maxScore outcome, depth, [])


maxResult :: Result -> Result -> Result
maxResult r1@(s1, d1, ps1) r2@(s2, d2, ps2)
  | s1 > s2 = r1
  | s2 > s1 = r2
  | d1 < d2 = r1
  | d2 < d1 = r2
  | otherwise = (s1, d1, ps1 ++ ps2)


minResult :: Result -> Result -> Result
minResult r1@(s1, d1, ps1) r2@(s2, d2, ps2)
  | s1 < s2 = r1
  | s2 < s1 = r2
  | d1 < d2 = r1
  | d2 < d1 = r2
  | otherwise = (s1, d1, ps1 ++ ps2)


maxScore :: Outcome -> Score
maxScore Win = 2
maxScore Squash = 1


minScore :: Outcome -> Score
minScore = negate . maxScore
