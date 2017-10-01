module FoldsSpec where

import           Adts
import           Folds

import           Data.Foldable (toList)

import           Test.Hspec

spec :: Spec
spec = do
    it "Tree Foldable" $
        toList (fromList [6, 2, 4, 3, 8]) `shouldBe` [2, 3, 4, 6, 8]

    it "splitOn" $
        splitOn '/' "path/to/file" `shouldBe` ["path", "to", "file"]

    it "joinWith" $
        joinWith '/' ["path", "to", "file"] `shouldBe` "path/to/file"
