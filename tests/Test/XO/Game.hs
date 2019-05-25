module Test.XO.Game (spec) where


import Test.Hspec

import XO.Game as Game
import XO.Grid as Grid
import XO.Mark


spec :: Spec
spec =
  describe "play" $ do
    context "when position is out of bounds" $ do
      it "returns Left OutOfBounds" $ do
        let Left error = Game.play (0,4) (Game.new X)

        error `shouldBe` OutOfBounds

    context "when position is unavailable" $ do
      it "returns Left Unavailable" $ do
        let Left error = Game.play (1,1) (Game.new X) >>= Game.play (1,1)

        error `shouldBe` Unavailable

    context "when 3 successful moves have been made" $ do
      it "returns a game in the playing state" $ do
        let Right game = Game.play (1,1) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (2,0)

        Game.outcome game `shouldBe` Nothing

    context "when X wins" $ do
      it "returns a game in the game over state" $ do
        let Right game = Game.play (1,1) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (2,0) >>=
                               Game.play (1,2) >>=
                                 Game.play (2,2) >>=
                                   Game.play (2,1) >>=
                                     Game.play (0,0)

        Game.turn game `shouldBe` X
        Game.outcome game `shouldBe` Just Win
