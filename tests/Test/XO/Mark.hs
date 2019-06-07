module Test.XO.Mark (spec) where


import Test.Hspec

import XO.Mark as Mark


spec :: Spec
spec = do
  showSpec
  swapSpec


showSpec :: Spec
showSpec =
  describe "show" $ do
    context "when X" $ do
      it "returns \"x\"" $ do
        show X `shouldBe` "x"

    context "when O" $ do
      it "returns \"o\"" $ do
        show O `shouldBe` "o"


swapSpec :: Spec
swapSpec =
  describe "swap" $ do
    context "when X" $ do
      it "returns O" $ do
        Mark.swap X `shouldBe` O

    context "when O" $ do
      it "returns X" $ do
        Mark.swap O `shouldBe` X
