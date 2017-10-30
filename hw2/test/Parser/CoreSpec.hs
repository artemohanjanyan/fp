module Parser.CoreSpec where

import           Parser.Core

import           Test.Hspec

import           Data.Char   (isUpper)

spec :: Spec
spec = do
    it "eof" $ do
        runParser eof " " `shouldBe` Left ParseError
        runParser eof "" `shouldBe` Right ("", ())

    it "satisfy" $ do
        runParser (satisfy isUpper) "ABC" `shouldBe` Right ("BC", 'A')
        runParser (satisfy isUpper) "aBC" `shouldBe` Left ParseError
