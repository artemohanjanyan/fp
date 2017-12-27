module BaseLensSpec where

import           BaseLens

import           Test.Hspec

spec :: Spec
spec = do
    it "pair" $ do
        set _1 'a' (1, "asd") `shouldBe` ('a', "asd")
        set _2 'a' (1, "asd") `shouldBe` (1, 'a')

        view _1 (1, "asd") `shouldBe` 1
        view _2 (1, "asd") `shouldBe` "asd"

        over _1 (+1)   (1, "asd") `shouldBe` (2, "asd")
        over _2 ('q':) (1, "asd") `shouldBe` (1, "qasd")
