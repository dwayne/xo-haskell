module Test.XO.AI (spec) where


import Test.Hspec

import XO.AI as AI
import XO.Game as Game
import XO.Mark


spec :: Spec
spec =
  describe "getPositions" $ do
    context "when there is a position to block and losing isn't inevitable" $ do
      it "returns the blocking position" $ do
        let Right game = Game.play (0,0) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (1,1)

        -- N.B. By blocking a squash can be forced with perfect play.
        AI.getPositions game `shouldBe` [(2,2)]

    context "when there is a position to block and losing is inevitable" $ do
      it "doesn't return the blocking position" $ do
        -- Q: Why?
        -- A: Because to block would extend the game unnecessarily. Just let
        --    the opponent win in the next move and end the game quickly.

        let Right game = Game.play (0,0) (Game.new X) >>=
                           Game.play (0,1) >>=
                             Game.play (1,1)

        -- N.B. The blocking position is (2,2).
        AI.getPositions game `shouldBe` [(0,2), (1,0), (1,2), (2,0), (2,1)]

    context "when there is a position to win" $ do
      it "returns the winning position" $ do
        let Right game = Game.play (0,0) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (1,0) >>=
                               Game.play (2,1)

        AI.getPositions game `shouldBe` [(2,0)]

    context "when there is a position to block and a position to win" $ do
      it "returns the winning position" $ do
        let Right game = Game.play (2,0) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (0,0) >>=
                               Game.play (2,2)

        AI.getPositions game `shouldBe` [(1,0)]
