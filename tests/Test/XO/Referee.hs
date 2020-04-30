module Test.XO.Referee (spec) where


import Test.Hspec

import Test.Helper

import XO.Grid as Grid
import XO.Mark
import XO.Referee as Referee


spec :: Spec
spec =
  describe "unsafeDecide" $ do
    context "when X wins" $ do
      it "returns Just Win" $ do
        let grid = setPositions X [(0, 0), (1, 0), (0, 1), (1, 1), (0, 2)]

        Referee.unsafeDecide grid X `shouldBe` Just Win

    context "when O wins" $ do
      it "returns Just Win" $ do
        let grid = setPositions O [(2, 0), (1, 0), (2, 1), (1, 1), (2, 2)]

        Referee.unsafeDecide grid O `shouldBe` Just Win

    context "when squashed" $ do
      it "returns Just Squash" $ do
        let grid = setPositions X [ (0, 0), (1, 1), (2, 2)
                                  , (0, 1), (2, 1), (2, 0)
                                  , (0, 2), (1, 2), (1, 0)
                                  ]

        Referee.unsafeDecide grid X `shouldBe` Just Squash

    context "after 2 plays" $ do
      it "returns Nothing" $ do
        let grid = setPositions X [(0, 0), (1, 1)]

        Referee.unsafeDecide grid O `shouldBe` Nothing
