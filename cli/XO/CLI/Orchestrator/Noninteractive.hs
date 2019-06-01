module XO.CLI.Orchestrator.Noninteractive (run) where


import XO.AI as AI
import XO.Game as Game
import XO.Mark

import XO.CLI.Orchestrator.Common


run :: Mark -> Int -> IO ()
run first rounds = runLoop rounds (Game.new first)


runLoop :: Int -> Game -> IO ()
runLoop rounds game
  | rounds == 0 = return ()
  | otherwise = do
    finalGame <- playOneGame game
    if rounds == 1 then
      putStrLn ""
    else
      runLoop (rounds-1) (Game.renew finalGame)


playOneGame :: Game -> IO Game
playOneGame game = do
  nextGame <- playOneTurn game

  case Game.outcome nextGame of
    Nothing ->
      playOneGame nextGame

    Just outcome ->
      handleGameOver outcome nextGame


playOneTurn :: Game -> IO Game
playOneTurn game = return nextGame
  where
    Just position = AI.getPosition game
    Right nextGame = Game.play position game


handleGameOver :: Outcome -> Game -> IO Game
handleGameOver outcome game = do
  case outcome of
    Win ->
      putStr (displayMark (Game.turn game))

    Squash ->
      putStr "."

  return game
