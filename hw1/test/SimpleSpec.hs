module SimpleSpec where

import           Simple

import           Test.Hspec

spec :: Spec
spec = do
    it "order3" $ do
        order3 (3, 2, 1)  `shouldBe` (1, 2, 3)
        order3 (2, 3, 1)  `shouldBe` (1, 2, 3)
        order3 (1, 2, 3)  `shouldBe` (1, 2, 3)
        order3 (5, 2, 10) `shouldBe` (2, 5, 10)

    it "highestBit" $ do
        highestBit 15 `shouldBe` 8
        highestBit 16 `shouldBe` 16
        highestBit 17 `shouldBe` 16

    it "highestBitHard" $ do
        highestBitHard 15 `shouldBe` (8,  3)
        highestBitHard 16 `shouldBe` (16, 4)
        highestBitHard 17 `shouldBe` (16, 4)

    it "smartReplicate" $
        smartReplicate [1, 2, 3] `shouldBe` [1, 2, 2, 3, 3, 3]

    it "contains" $
        contains 3 [[1..5], [2, 0], [3, 4]] `shouldBe` [[1,2,3,4,5],[3,4]]
