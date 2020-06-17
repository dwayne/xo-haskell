module XO.CLI.Random (randomElem) where


import qualified System.Random as Random


randomElem :: [a] -> IO a
randomElem xs = rand >>= (return . ((!!) xs))
  where
    rand = Random.randomRIO (0, n-1)
    n = length xs
