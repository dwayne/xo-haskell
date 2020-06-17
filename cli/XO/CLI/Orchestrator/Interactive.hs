module XO.CLI.Orchestrator.Interactive (run) where


import Control.Monad (when)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.Read as Read

import qualified XO.AI as AI
import XO.Game as Game
import XO.Grid as Grid
import XO.Mark

import XO.CLI.Player as Player
import XO.CLI.Random (randomElem)


run :: Mark -> Player -> Player -> IO ()
run first x o =
  putStrLn showIntro >> runLoop x o (Player.numHumans x o) (Game.new first)


runLoop :: Player -> Player -> Int -> Game -> IO ()
runLoop x o humans game = do
  finalGame <-
    case Game.turn game of
      X -> playOneGame x o humans game
      O -> playOneGame o x humans game

  playing <- getContinue

  when playing $ runLoop x o humans (Game.renew finalGame)


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
  putStrLn $ showTurn humans (Game.turn game)
  putStrLn $ showGrid (Game.grid game)

  playOneTurnLoop game

playOneTurn Computer humans game = do
  position <- randomElem $ AI.getPositions game

  putStrLn $ "The computer played at " ++ showPosition position

  let Right nextGame = Game.play position game
  return nextGame


playOneTurnLoop :: Game -> IO Game
playOneTurnLoop game = do
  position <- getPosition (Game.grid game) True

  case Game.play position game of
    Left OutOfBounds -> do
      putStrLn "Try again, that position is out of bounds"
      playOneTurnLoop game

    Left Unavailable -> do
      putStrLn "Try again, that position is already taken"
      playOneTurnLoop game

    Right nextGame ->
      return nextGame


handleGameOver :: Outcome -> Player -> Int -> Game -> IO Game
handleGameOver outcome player humans game = do
  case (outcome, player, humans) of
    (Win, Human, 2) ->
      putStrLn $ "Congratulations! " ++ show (Game.turn game) ++ " won."

    (Win, Human, 1) ->
      putStrLn "Congratulations! You won."

    (Win, Computer, 1) ->
      putStrLn "The computer won. Better luck next time."

    (Squash, _, _) ->
      putStrLn "Game squashed."

  putStrLn $ showGrid (Game.grid game)

  return game


-- INPUT


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
        putStrLn $ "where 1 <= r <= 3 and 1 <= c <= 3, for e.g. \"" ++
          show r ++ " " ++ show c ++ "\""

        getPosition grid False
      else
        getPosition grid showHelp

    Just position ->
      return position


firstAvailablePosition :: Grid -> Position
firstAvailablePosition = increment . head . Grid.availablePositions


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
          Just (decrement (r, c))

        _ ->
          Nothing

    _ ->
      Nothing


parseInt :: String -> Maybe Int
parseInt input = Read.readMaybe input


-- OUTPUT


showIntro :: String
showIntro =
  List.intercalate "\n"
    [ "Welcome to Tic-tac-toe"
    , "Play as many games as you want"
    , "Press Ctrl-C to exit at any time"
    , ""
    ]


showTurn :: Int -> Mark -> String
showTurn humans mark =
  if humans == 2 then
    show mark ++ "'s turn"
  else
    "Your turn (" ++ show mark ++ ")"


showGrid :: Grid -> String
showGrid = showTiles . Grid.toList
  where
    showTiles [a, b, c, d, e, f, g, h, i] =
      List.intercalate "\n"
        [ showRow a b c
        , showSep
        , showRow d e f
        , showSep
        , showRow g h i
        ]

    showRow a b c =
      List.intercalate "|"
        [ showTile a
        , showTile b
        , showTile c
        ]

    showTile Nothing  = "   "
    showTile (Just X) = " X "
    showTile (Just O) = " O "

    showSep = "---+---+---"


showPosition :: Position -> String
showPosition p = show r ++ ", " ++ show c
  where
    (r, c) = increment p


-- HELPERS


increment :: Position -> Position
increment (r, c) = (r+1, c+1)


decrement :: Position -> Position
decrement (r, c) = (r-1, c-1)
