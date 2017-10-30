module Parser.LetsSpec where

import           Parser.Core
import           Parser.Lets

import           Test.Hspec

import qualified Data.Map    as Map

spec :: Spec
spec = it "parseSExpr" $ do
    runParser (simplify Map.empty) "let x = 1 + 2 + 5\nlet   y = x+x\nlet z=0+    x   + y + 8"
            `shouldBe` Right ("", Let [("x", 8), ("y", 16), ("z", 32)])
