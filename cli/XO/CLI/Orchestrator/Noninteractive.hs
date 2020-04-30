module XO.CLI.Orchestrator.Noninteractive (run) where


import Data.Maybe (maybe)
import qualified System.Random as Random

import XO.AI (getPositions)
import XO.Game (Game, Outcome(Win, Squash), new, outcome, play, renew, turn)
import XO.Mark (Mark)


run :: Mark -> Int -> IO ()
run = runLoop . new


runLoop :: Game -> Int -> IO ()
runLoop game rounds
  | rounds == 0 = return ()
  | otherwise   = do
    finalGame <- playOneGame game
    if rounds == 1 then
      putStrLn ""
    else
      runLoop (renew finalGame) (rounds-1)


playOneGame :: Game -> IO Game
playOneGame game = do
  nextGame <- playOneTurn game
  maybe (playOneGame nextGame) (handleGameOver nextGame) (outcome nextGame)


playOneTurn :: Game -> IO Game
playOneTurn game = do
  position <- randomElem $ getPositions game
  let Right nextGame = play position game
  return nextGame


handleGameOver :: Game -> Outcome -> IO Game
handleGameOver game outcome = putStr (showOutcome outcome game) >> return game


showOutcome :: Outcome -> Game -> String
showOutcome Win    = show . turn
showOutcome Squash = const "."


-- HELPERS


randomElem :: [a] -> IO a
randomElem xs = rand >>= (return . ((!!) xs))
  where
    rand = Random.getStdRandom (Random.randomR (0, n-1))
    n = length xs
