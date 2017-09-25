module PatternsSpec where

import Patterns

import Test.Hspec

spec :: Spec
spec = do
    it "removeAt" $ do
        removeAt 1  [1, 2, 3] `shouldBe` [1, 3]
        removeAt 10 [1, 2, 3] `shouldBe` [1, 2, 3]
        removeAt 3  [1..5]    `shouldBe` [1, 2, 3, 5]
        removeAt 2  "abc"     `shouldBe` "ab"

    it "removeAtHard" $ do
        removeAtHard 1  [1, 2, 3] `shouldBe` (Just 2, [1, 3])
        removeAtHard 10 [1, 2, 3] `shouldBe` (Nothing, [1, 2, 3])
        removeAtHard 3  [1..5]    `shouldBe` (Just 4, [1, 2, 3, 5])
        removeAtHard 2  "abc"     `shouldBe` (Just 'c', "ab")

    it "collectEvery" $ do
        collectEvery 3 [1..8] `shouldBe` ([1, 2, 4, 5, 7, 8], [3, 6])

    it "stringSum" $ do
        stringSum "1"                          `shouldBe` Just 1
        stringSum "1 2 3"                      `shouldBe` Just 6
        stringSum " 1"                         `shouldBe` Just 1
        stringSum "1 "                         `shouldBe` Just 1
        stringSum "\t1\t"                      `shouldBe` Just 1
        stringSum "\t12345\t"                  `shouldBe` Just 12345
        stringSum "010 020 030"                `shouldBe` Just 60
        stringSum " 123 456 789 "              `shouldBe` Just 1368
        stringSum "-1"                         `shouldBe` Just (-1)
        stringSum "-1 -2 -3"                   `shouldBe` Just (-6)
        stringSum "\t-12345\t"                 `shouldBe` Just (-12345)
        stringSum " -123 -456 -789 "           `shouldBe` Just (-1368)
        stringSum "\n1\t\n3   555  -1\n\n\n-5" `shouldBe` Just 553
        stringSum "123\t\n\t\n\t\n321 -4 -40"  `shouldBe` Just 400

    it "stringSum parse error" $ do
        stringSum "asd" `shouldBe` Nothing
        stringSum "1-1" `shouldBe` Nothing
        stringSum "1.2" `shouldBe` Nothing
        stringSum "--2" `shouldBe` Nothing
        stringSum "1+"  `shouldBe` Nothing
        --stringSum "+1"  `shouldBe` Nothing

    it "stringSumHard" $ do
        stringSumHard "+1"    `shouldBe` Just 1
        stringSumHard "1 +1"  `shouldBe` Just 2
        stringSumHard "-1 +1" `shouldBe` Just 0
        stringSumHard "+1 -1" `shouldBe` Just 0

    it "stringSumHard parse error" $ do
        stringSumHard "1+1"   `shouldBe` Nothing
        stringSumHard "++1"   `shouldBe` Nothing
        stringSumHard "-+1"   `shouldBe` Nothing
        stringSumHard "+-1"   `shouldBe` Nothing
        stringSumHard "1 + 1" `shouldBe` Nothing

    it "mergeSort" $ do
        mergeSort [2, 1, 0, 3, 10, 5] `shouldBe` [0, 1, 2, 3, 5, 10]
