module XO.CLI.Orchestrator.Interactive (run) where


import Control.Monad (when)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.Read as Read

import XO.Game as Game
import XO.Grid as Grid
import XO.Mark as Mark

import XO.CLI.Player as Player


run :: Player -> Player -> Mark -> IO ()
run x o first =
  putStrLn displayIntro >> runLoop x o (Player.numHumans x o) (Game.new first)


runLoop :: Player -> Player -> Int -> Game -> IO ()
runLoop x o humans game = do
  finalGame <-
    case Game.turn game of
      X -> playOneGame x o humans game
      O -> playOneGame o x humans game

  playing <- getContinue

  when playing (runLoop x o humans (Game.renew finalGame))


playOneGame :: Player -> Player -> Int -> Game -> IO Game
playOneGame currentPlayer nextPlayer humans game = do
  nextGame <- playOneTurn currentPlayer humans game

  case Game.outcome nextGame of
    Nothing ->
      playOneGame nextPlayer currentPlayer humans nextGame

    Just outcome ->
      handleGameOver outcome currentPlayer humans nextGame


playOneTurn :: Player -> Int -> Game -> IO Game
playOneTurn Human humans game = do
  let mark = Game.turn game
  let grid = Game.grid game

  if humans == 2 then
    putStrLn (displayMark mark ++ "'s turn")
  else
    putStrLn ("Your turn (" ++ displayMark mark ++ ")")

  putStrLn (displayGrid grid)

  playOneTurnLoop grid game

playOneTurn Computer humans game =
  return game


playOneTurnLoop :: Grid -> Game -> IO Game
playOneTurnLoop grid game = do
  position <- getPosition grid True

  case Game.play position game of
    Left OutOfBounds -> do
      putStrLn "Try again, that position is out of bounds"
      playOneTurnLoop grid game

    Left Unavailable -> do
      putStrLn "Try again, that position is already taken"
      playOneTurnLoop grid game

    Right nextGame ->
      return nextGame


handleGameOver :: Outcome -> Player -> Int -> Game -> IO Game
handleGameOver outcome player humans game = do
  case (outcome, player, humans) of
    (Win, Human, 2) ->
      putStrLn ("Congratulations! " ++ displayMark (Game.turn game) ++ " won.")

    (Win, Human, 1) ->
      putStrLn "Congratulations! You won."

    (Win, Computer, 1) ->
      putStrLn "The computer won. Better luck next time."

    (Squash, _, _) ->
      putStrLn "Game squashed."

  putStrLn (displayGrid (Game.grid game))

  return game


getContinue :: IO Bool
getContinue = do
  input <- getInput "Do you want to continue playing? (Y/n) "
  let s = map Char.toLower input

  if s == "" || s == "y" || s == "yes" then
    return True
  else if s == "n" || s == "no" then
    return False
  else
    getContinue


getPosition :: Grid -> Bool -> IO Position
getPosition grid showHelp = do
  s <- getInput "> "
  case parsePosition s of
    Nothing ->
      if showHelp then do
        let (r, c) = firstAvailablePosition grid

        putStrLn "Try again, but this time enter a position in the format \
          \\"r c\","
        putStrLn ("where 1 <= r <= 3 and 1 <= c <= 3, for e.g. \"" ++
          show r ++ " " ++ show c ++ "\"")

        getPosition grid False
      else
        getPosition grid showHelp

    Just position ->
      return position


getInput :: String -> IO String
getInput prompt = putStr prompt >> getLine >>= (return . strip)
  where
    lstrip = dropWhile Char.isSpace
    rstrip = reverse . lstrip . reverse
    strip = rstrip . lstrip


parsePosition :: String -> Maybe Position
parsePosition input =
  case words input of
    [a, b] ->
      case (parseInt a, parseInt b) of
        (Just r, Just c) ->
          Just (r-1, c-1)

        _ ->
          Nothing

    _ ->
      Nothing


parseInt :: String -> Maybe Int
parseInt input = Read.readMaybe input


firstAvailablePosition :: Grid -> Position
firstAvailablePosition = incr . head . Grid.availablePositions
  where
    incr (r, c) = (r+1, c+1)


displayIntro :: String
displayIntro =
  List.intercalate "\n"
    [ "Welcome to Tic-tac-toe"
    , "Play as many games as you want"
    , "Press Ctrl-C to exit at any time"
    , ""
    ]


displayMark :: Mark -> String
displayMark X = "x"
displayMark O = "o"


displayGrid :: Grid -> String
displayGrid = displayTiles . Grid.toList
  where
    displayTiles [a, b, c, d, e, f, g, h, i] =
      List.intercalate "\n"
        [ displayRow a b c
        , displaySep
        , displayRow d e f
        , displaySep
        , displayRow g h i
        ]

    displayRow a b c =
      List.intercalate "|"
        [ displayTile a
        , displayTile b
        , displayTile c
        ]

    displayTile Nothing = "   "
    displayTile (Just X) = " x "
    displayTile (Just O) = " o "

    displaySep = "---+---+---"
