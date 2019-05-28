module XO.Game
  ( Game, Outcome(..), Error(..)
  , new
  , play, renew
  , grid, turn, outcome
  )
  where


import XO.Grid as Grid
import XO.Mark as Mark
import XO.Referee as Referee


data Game
  = Playing Grid Mark
  | GameOver Grid Mark Outcome


data Error
  = OutOfBounds
  | Unavailable
  deriving (Eq, Show)


new :: Mark -> Game
new = Playing Grid.empty


play :: Position -> Game -> Either Error Game
play p (Playing grid mark) =
  if Grid.inBounds p then
    if Grid.isAvailable p grid then
      Right (playMark p mark grid)
    else
      Left Unavailable
  else
    Left OutOfBounds
play _ game = Right game


playMark :: Position -> Mark -> Grid -> Game
playMark p mark grid =
  let
    nextGrid = Grid.set p mark grid
  in
    case Referee.unsafeDecide nextGrid mark of
      Nothing ->
        Playing nextGrid (Mark.swap mark)

      Just outcome ->
        GameOver nextGrid mark outcome


renew :: Game -> Game
renew (Playing _ m) = new m
renew (GameOver _ m Win) = new m
renew (GameOver _ m Squash) = new (Mark.swap m)


grid :: Game -> Grid
grid (Playing g _) = g
grid (GameOver g _ _) = g


turn :: Game -> Mark
turn (Playing _ m) = m
turn (GameOver _ m _) = m


-- An outcome exists if and only if the game is over.
outcome :: Game -> Maybe Outcome
outcome (GameOver _ _ o) = Just o
outcome _ = Nothing
