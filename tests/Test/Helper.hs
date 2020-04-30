module Test.Helper (setPositions, playPositions) where


import Control.Monad (foldM)

import XO.Mark
import XO.Grid
import XO.Game


setPositions :: Mark -> [Position] -> Grid
setPositions first ps =
  foldl (\g (p, m) -> set p m g) empty (zip ps (iterate swap first))


playPositions :: Mark -> [Position] -> Either Error Game
playPositions first = foldM (flip play) (new first)
