module Main where


import Test.Hspec

import qualified Test.XO.AI
import qualified Test.XO.Game
import qualified Test.XO.Grid
import qualified Test.XO.Mark
import qualified Test.XO.Referee


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "XO.AI" Test.XO.AI.spec
  describe "XO.Game" Test.XO.Game.spec
  describe "XO.Grid" Test.XO.Grid.spec
  describe "XO.Mark" Test.XO.Mark.spec
  describe "XO.Referee" Test.XO.Referee.spec
