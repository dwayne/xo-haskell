module Test.XO.Game (spec) where


import Test.Hspec

import XO.Game as Game
import XO.Grid as Grid
import XO.Mark


spec :: Spec
spec = do
  newSpec
  playSpec
  renewSpec


newSpec :: Spec
newSpec =
  describe "new" $ do
    context "with X" $ do
      it "is X's turn and the grid is empty" $ do
        let game = Game.new X

        Game.turn game `shouldBe` X
        Game.grid game `shouldBe` Grid.empty
        Game.outcome game `shouldBe` Nothing

    context "with O" $ do
      it "is O's turn and the grid is empty" $ do
        let game = Game.new O

        Game.turn game `shouldBe` O
        Game.grid game `shouldBe` Grid.empty
        Game.outcome game `shouldBe` Nothing


playSpec :: Spec
playSpec =
  describe "play" $ do
    context "when position is in bounds and available" $ do
      context "after 3 plays" $ do
        it "returns an in-progress game" $ do
          let Right game = Game.play (1,1) (Game.new X) >>=
                             Game.play (0,2) >>=
                               Game.play (2,0)

          Game.turn game `shouldBe` O
          show (Game.grid game) `shouldBe` "..o.x.x.."
          Game.outcome game `shouldBe` Nothing

      context "when X wins" $ do
        it "returns a completed game in which X is the winner" $ do
          let Right game = Game.play (1,1) (Game.new X) >>=
                             Game.play (0,2) >>=
                               Game.play (2,0) >>=
                                 Game.play (1,2) >>=
                                   Game.play (2,2) >>=
                                     Game.play (2,1) >>=
                                       Game.play (0,0)

          Game.turn game `shouldBe` X
          Game.outcome game `shouldBe` Just Win

      context "when O squashes" $ do
        it "returns a completed game in which O squashed" $ do
          let Right game = Game.play (1,1) (Game.new O) >>=
                             Game.play (0,0) >>=
                               Game.play (2,2) >>=
                                 Game.play (0,2) >>=
                                   Game.play (0,1) >>=
                                     Game.play (2,1) >>=
                                       Game.play (1,2) >>=
                                         Game.play (1,0) >>=
                                           Game.play (2,0)

          Game.turn game `shouldBe` O
          Game.outcome game `shouldBe` Just Squash

    context "when position is out of bounds" $ do
      it "returns Left OutOfBounds" $ do
        let Left error = Game.play (0,4) (Game.new X)

        error `shouldBe` OutOfBounds

    context "when position is unavailable" $ do
      it "returns Left Unavailable" $ do
        let Left error = Game.play (1,1) (Game.new X) >>= Game.play (1,1)

        error `shouldBe` Unavailable


renewSpec :: Spec
renewSpec =
  describe "renew" $ do
    context "when X plays first and after 3 plays" $ do
      it "is O's turn and the grid is empty" $ do
        let Right game = Game.play (1,1) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (2,0)
        let newGame = Game.renew game

        Game.turn newGame `shouldBe` O
        Game.grid newGame `shouldBe` Grid.empty
        Game.outcome newGame `shouldBe` Nothing

    context "when X wins" $ do
      it "is X's turn and the grid is empty" $ do
        let Right game = Game.play (1,1) (Game.new X) >>=
                           Game.play (0,2) >>=
                             Game.play (2,0) >>=
                               Game.play (1,2) >>=
                                 Game.play (2,2) >>=
                                   Game.play (2,1) >>=
                                     Game.play (0,0)
        let newGame = Game.renew game

        Game.turn newGame `shouldBe` X
        Game.grid newGame `shouldBe` Grid.empty
        Game.outcome newGame `shouldBe` Nothing

    context "when O squashes" $ do
      it "is X's turn and the grid is empty" $ do
        let Right game = Game.play (1,1) (Game.new O) >>=
                           Game.play (0,0) >>=
                             Game.play (2,2) >>=
                               Game.play (0,2) >>=
                                 Game.play (0,1) >>=
                                   Game.play (2,1) >>=
                                     Game.play (1,2) >>=
                                       Game.play (1,0) >>=
                                         Game.play (2,0)
        let newGame = Game.renew game

        Game.turn newGame `shouldBe` X
        Game.grid newGame `shouldBe` Grid.empty
        Game.outcome newGame `shouldBe` Nothing
