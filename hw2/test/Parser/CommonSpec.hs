module Parser.CommonSpec where

import           Parser.Common
import           Parser.Core

import           Test.Hspec

import           Control.Applicative (Applicative (..))

spec :: Spec
spec = do
    it "char" $ do
        runParser (char 'a') "abc" `shouldBe` Right ("bc", 'a')
        runParser (char 'a') "bc" `shouldBe` Left ParseError

    it "abParser" $ do
        runParser abParser "ab" `shouldBe` Right ("", ('a', 'b'))
        runParser abParser "ac" `shouldBe` Left ParseError

    it "abParser_" $ do
        runParser abParser_ "ab" `shouldBe` Right ("", ())
        runParser abParser_ "ac" `shouldBe` Left ParseError

    it "posInt" $ do
        runParser (posInt <* eof) "123" `shouldBe` Right ("", 123)
        runParser (posInt <* eof) "12a" `shouldBe` Left ParseError

    it "space" $ do
        runParser space "  a" `shouldBe` Right ("a", ())
        runParser space "a" `shouldBe` Right ("a", ())

    it "space1" $ do
        runParser space1 "  a" `shouldBe` Right ("a", ())
        runParser space1 "a" `shouldBe` Left ParseError

    it "intPair" $ do
        runParser intPair "1 \t  \n 2" `shouldBe` Right ("", (1, 2))
        runParser intPair " 2" `shouldBe` Left ParseError

    it "intOrUppercase" $ do
        runParser intOrUppercase "1" `shouldBe` Right ("", ())
        runParser intOrUppercase "A" `shouldBe` Right ("", ())
        runParser intOrUppercase "a" `shouldBe` Left ParseError

    it "zeroOrMore" $ do
        runParser (zeroOrMore $ char 'a') "aaa " `shouldBe` Right (" ", "aaa")
        runParser (zeroOrMore $ char 'a') " " `shouldBe` Right (" ", "")

    it "oneOrMore" $ do
        runParser (oneOrMore $ char 'a') "aaa " `shouldBe` Right (" ", "aaa")
        runParser (oneOrMore $ char 'a') " " `shouldBe` Left ParseError

    it "spaces" $ do
        runParser spaces "  a" `shouldBe` Right ("a", "  ")
        runParser spaces "a" `shouldBe` Right ("a", "")

    it "ident" $ do
        runParser ident "Aa123$" `shouldBe` Right ("$", "Aa123")
        runParser ident "1Aa123$" `shouldBe` Left ParseError

    it "sepBy" $ do
        runParser (sepBy (char 'a') space1) "a   a aa"
                `shouldBe` Right ("a", ['a', 'a', 'a'])
        runParser (sepBy (char 'a') space1) "  "
                `shouldBe` Left ParseError
