module Main where


import Test.Hspec

import qualified Test.XO.Game
import qualified Test.XO.Referee


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "XO.Game" Test.XO.Game.spec
  describe "XO.Referee" Test.XO.Referee.spec
