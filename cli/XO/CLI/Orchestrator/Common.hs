module XO.CLI.Orchestrator.Common
  ( getRandomPosition
  , displayMark
  )
  where


import qualified System.Random as Random

import XO.Grid as Grid
import XO.Mark


getRandomPosition :: Grid -> IO Position
getRandomPosition grid = do
  let availablePositions = Grid.availablePositions grid
  let n = length availablePositions

  i <- Random.getStdRandom (Random.randomR (0, n-1))

  return (availablePositions !! i)


displayMark :: Mark -> String
displayMark X = "x"
displayMark O = "o"
