module Test.XO.Grid (spec) where


import Test.Hspec

import Test.Helper

import Data.Maybe (isNothing)

import XO.Mark
import XO.Grid as Grid


spec :: Spec
spec = do
  emptySpec
  setSpec
  isAvailableSpec
  inBoundsSpec
  availablePositionsSpec
  toListSpec


emptySpec :: Spec
emptySpec = do
  describe "empty" $ do
    it "creates an empty grid" $ do
      let actual = all isNothing (Grid.toList Grid.empty)

      actual `shouldBe` True


setSpec :: Spec
setSpec = do
  describe "set" $ do
    context "when position is (1, 1) and mark is X" $ do
      it "returns a new grid with an X at (1, 1)" $ do
        let grid = setPositions X [(1, 1)]
        let actual = head (drop 4 (Grid.toList grid))

        actual `shouldBe` Just X


isAvailableSpec :: Spec
isAvailableSpec = do
  describe "isAvailable" $ do
    context "when (0, 1) does not contain a mark" $ do
      it "returns True" $ do
        let actual = Grid.isAvailable (0, 1) Grid.empty

        actual `shouldBe` True

    context "when an O is at (0, 1)" $ do
      it "returns False" $ do
        let grid = setPositions O [(0, 1)]
        let actual = Grid.isAvailable (0, 1) grid

        actual `shouldBe` False


inBoundsSpec :: Spec
inBoundsSpec = do
  describe "inBounds" $ do
    context "when 0 <= r <= 2 and 0 <= c <= 2" $ do
      it "returns True" $ do
        let positions = [(r, c) | r <- [0..2], c <- [0..2]]
        let actual = and (map Grid.inBounds positions)

        actual `shouldBe` True

    context "when r < 0 or r > 2 or c < 0 or c > 2" $ do
      it "returns False" $ do
        let positions = [ (-1, 0), (3, 0), (0, -1), (0, 3)
                        , (-1, -1), (-1, 3), (3, -1), (3, 3)
                        ]
        let actual = or (map Grid.inBounds positions)

        actual `shouldBe` False


availablePositionsSpec :: Spec
availablePositionsSpec = do
  describe "availablePositions" $ do
    context "when the grid is empty" $ do
      it "returns all positions in row-major order" $ do
        let actual = Grid.availablePositions Grid.empty
        let expected = [(r, c) | r <- [0..2], c <- [0..2]]

        actual `shouldBe` expected

    context "when (0, 0), (0, 2), (1, 1), (2, 0) and (2, 2) are marked" $ do
      it "returns the unmarked positions (0, 1), (1, 0), (1, 2) and (2, 1) in that order" $ do
        let grid = setPositions X [(2, 0), (0, 2), (1, 1), (2, 2), (0, 0)]
        let actual = Grid.availablePositions grid
        let expected = [(0, 1), (1, 0), (1, 2), (2, 1)]

        actual `shouldBe` expected


toListSpec :: Spec
toListSpec = do
  describe "toList" $ do
    it "returns a list of tiles in row-major order" $ do
      let actual = Grid.toList $ setPositions X [(0, 0), (1, 1)]
      let expected = [ Just X, Nothing, Nothing
                     , Nothing, Just O, Nothing
                     , Nothing, Nothing, Nothing
                     ]

      actual `shouldBe` expected
