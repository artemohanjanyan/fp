module OptionalsSpec where

import           Optionals

import           Test.Hspec

spec :: Spec
spec = do
    describe "expr" $ do
        let c :: Int -> Expr Int
            c = Const
        it "eval Right" $ do
            eval (c 2 :+: c 2 :*: c 2) `shouldBe` Right 6
            eval (c 2 :-: c 2 :/: c 2) `shouldBe` Right 1
            eval (c 3 :-: c 3 :^: c 3) `shouldBe` Right (-24)
        it "eval Left" $ do
            eval ((c 3 :-: c 3) :/: c 0) `shouldBe` Left DivisionByZero
            eval (c 3 :-: c 3 :^: c (-3)) `shouldBe` Left NegativeExponent
