module XO.AI (getPosition) where


import XO.Game as Game
import XO.Grid as Grid
import XO.Mark as Mark
import XO.Referee as Referee


type Score = Int
type Result = (Score, Maybe Position)


getPosition :: Game -> Maybe Position
getPosition game = snd (maximize max min (Game.grid game))
  where
    max = Game.turn game
    min = Mark.swap max


maximize :: Mark -> Mark -> Grid -> Result
maximize max min grid =
  case Referee.unsafeDecide grid min of
    Nothing ->
      let
        positions = Grid.availablePositions grid
        set p = Grid.set p max grid
        nextGrids = map set positions
        mins = map (minimize min max) nextGrids
        combine (r, p) = (fst r, Just p)
        nextResults = map combine (zip mins positions)
      in
        foldr1 maxResult nextResults

    Just outcome ->
      (minScore outcome, Nothing)


minimize :: Mark -> Mark -> Grid -> Result
minimize min max grid =
  case Referee.unsafeDecide grid max of
    Nothing ->
      let
        positions = Grid.availablePositions grid
        set p = Grid.set p min grid
        nextGrids = map set positions
        maxs = map (maximize max min) nextGrids
        combine (r, p) = (fst r, Just p)
        nextResults = map combine (zip maxs positions)
      in
        foldr1 minResult nextResults

    Just outcome ->
      (maxScore outcome, Nothing)


maxResult :: Result -> Result -> Result
maxResult r1@(s1, _) r2@(s2, _)
  | s1 >= s2 = r1
  | otherwise = r2


minResult :: Result -> Result -> Result
minResult r1@(s1, _) r2@(s2, _)
  | s1 <= s2 = r1
  | otherwise = r2


maxScore :: Outcome -> Score
maxScore Win = 2
maxScore Squash = 1


minScore :: Outcome -> Score
minScore = negate . maxScore
