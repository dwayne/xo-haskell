module Test.XO.Referee (spec) where


import Test.Hspec

import XO.Grid as Grid
import XO.Mark
import XO.Referee as Referee


spec :: Spec
spec =
  describe "unsafeDecide" $ do
    context "when X wins" $ do
      it "returns Just Win" $ do
        let grid = Grid.set (0,2) X $
                     Grid.set (1,1) O $
                       Grid.set (0,1) X $
                         Grid.set (1,0) O $
                           Grid.set (0,0) X empty

        Referee.unsafeDecide grid X `shouldBe` Just Win

    context "when O wins" $ do
      it "returns Just Win" $ do
        let grid = Grid.set (2,2) O $
                     Grid.set (1,1) X $
                       Grid.set (2,1) O $
                         Grid.set (1,0) X $
                           Grid.set (2,0) O empty

        Referee.unsafeDecide grid O `shouldBe` Just Win

    context "when squashed" $ do
      it "returns Just Squash" $ do
        let grid = Grid.set (1,0) X $
                     Grid.set (1,2) O $
                       Grid.set (0,2) X $
                         Grid.set (2,0) O $
                           Grid.set (2,1) X $
                             Grid.set (0,1) O $
                               Grid.set (2,2) X $
                                 Grid.set (1,1) O $
                                   Grid.set (0,0) X empty

        Referee.unsafeDecide grid X `shouldBe` Just Squash

    context "when it is too early to decide an outcome" $ do
      it "returns Nothing" $ do
        let grid = Grid.set (1,1) O (Grid.set (0,0) X empty)

        Referee.unsafeDecide grid X `shouldBe` Nothing
