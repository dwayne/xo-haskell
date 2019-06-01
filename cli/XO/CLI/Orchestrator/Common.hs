module XO.CLI.Orchestrator.Common
  ( randomElem
  , displayMark
  )
  where


import qualified System.Random as Random

import XO.Mark


randomElem :: [a] -> IO a
randomElem xs = rand >>= (return . ((!!) xs))
  where
    rand = Random.getStdRandom (Random.randomR (0, n-1))
    n = length xs


displayMark :: Mark -> String
displayMark X = "x"
displayMark O = "o"
