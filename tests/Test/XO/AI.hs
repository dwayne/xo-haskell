module Test.XO.AI (spec) where


import Test.Hspec

import XO.AI as AI
import XO.Game as Game
import XO.Mark


spec :: Spec
spec =
  describe "position selection" $ do
    context "when there is a position to block" $ do
      it "plays the blocking position" $ do
        let Right game = Game.play (0,0) (Game.new X) >>=
                           Game.play (0,1) >>=
                             Game.play (1,1)

        let Just position = AI.getPosition game

        position `shouldBe` (2,2)

    context "when there is a position to win" $ do
      it "plays the winning position" $ do
        let Right game = Game.play (0,0) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (1,0) >>=
                               Game.play (2,1)

        let Just position = AI.getPosition game

        position `shouldBe` (2,0)

    context "when there is a position to block and a position to win" $ do
      it "plays the winning position" $ do
        let Right game = Game.play (2,0) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (0,0) >>=
                               Game.play (2,2)

        let Just position = AI.getPosition game

        position `shouldBe` (1,0)
