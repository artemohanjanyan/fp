module AdtsSpec where

import Adts

import Data.Foldable (toList)

import Test.Hspec

spec :: Spec
spec = do
    describe "Day" $ do
        it "nextDay" $ do
            nextDay Tue `shouldBe` Wed
            nextDay Sat `shouldBe` Sun
            nextDay Sun `shouldBe` Mon

        it "afterDays" $ do
            afterDays Tue 2 `shouldBe` Thu
            afterDays Sat 2 `shouldBe` Mon

        it "isWeekend" $ do
            Tue `shouldSatisfy` not . isWeekend
            Sat `shouldSatisfy` isWeekend

        it "daysToParty" $ do
            daysToParty Sat `shouldBe` 6
            daysToParty Thu `shouldBe` 1

    it "fight" $ do
        fight (Knight "Arthur" "The Terrible" 100 50) (Monster 49 2) `shouldBe`
            (Left (Knight "Arthur" "The Terrible" 100 50), 0)
        fight (Knight "Arthur" "The Terrible" 100 50) (Monster 51 2) `shouldBe`
            (Left (Knight "Arthur" "The Terrible" 98 50), 2)

    describe "Vector" $ do
        it "euclidNorm" $ do
            euclidNorm (Vector2D 3 4) `shouldBe` 5

        it "add" $ do
            add (Vector2D 3 4) (Vector2D 5 6)   `shouldBe` (Vector2D 8 10)
            add (Vector2D 3 4) (Vector3D 5 6 7) `shouldBe` (Vector3D 8 10 7)

        it "scalar" $ do
            scalar (Vector2D 1 2) (Vector2D (-2) 1) `shouldBe` 0

        it "neg" $ do
            neg (Vector2D 1 2) `shouldBe` Vector2D (-1) (-2)

        it "dist" $ do
            dist (Vector2D 3 4) (Vector2D 7 7) `shouldBe` 5

        it "vectorProduct" $ do
            vectorProduct (Vector2D 3 4) (Vector2D 0 1) `shouldSatisfy`
                (> 0) . getZ
            vectorProduct (Vector2D 3 4) (Vector2D (-3) (-4)) `shouldSatisfy`
                (== 0) . getZ
            vectorProduct (Vector2D 0 1) (Vector2D 3 4) `shouldSatisfy`
                (< 0) . getZ

    describe "Nat" $ do
        let nat :: Integer -> Nat
            nat = fromInteger

        it "Nat + Nat" $ do
            nat 1 + nat 2 `shouldBe` nat 3

        it "Nat * Nat" $ do
            nat 4 * nat 9 `shouldBe` nat 36

        it "Nat - Nat" $ do
            nat 3 - nat 2 `shouldBe` nat 1

        it "fromInteger" $ do
            fromInteger 2 `shouldBe` S (S Z)

        it "toInteger" $ do
            toInteger (S (S Z)) `shouldBe` 2

        it "Eq" $ do
            nat 2 `shouldBe` nat 2
            nat 2 `shouldSatisfy` (/= nat 3)

        it "Ord" $ do
            nat 2 `shouldSatisfy` (< nat 3)
            nat 2 `shouldSatisfy` (> nat 1)

        it "even" $ do
            nat 2 `shouldSatisfy` even
            nat 3 `shouldSatisfy` not . even

        it "div" $ do
            div (nat 8) (nat 3) `shouldBe` nat 2
            div (nat 9) (nat 3) `shouldBe` nat 3

        it "mod" $ do
            mod (nat 8) (nat 3) `shouldBe` nat 2
            mod (nat 9) (nat 3) `shouldBe` nat 0

        it "gcd" $ do
            gcd (nat 12) (nat 8) `shouldBe` nat 4
            gcd (nat 13) (nat 8) `shouldBe` nat 1

    describe "Tree" $ do
        it "null" $ do
            (Leaf :: Tree Int) `shouldSatisfy` null
            Node 1 Leaf Leaf   `shouldSatisfy` not . null

        it "length" $ do
            length (Leaf :: Tree Int)                             `shouldBe` 0
            length (Node 1 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) `shouldBe` 3

        it "treeElem" $ do
            fromList [1..10] `shouldSatisfy` treeElem 5
            fromList [1..10] `shouldSatisfy` not . treeElem 0

        it "treeInsert" $ do
            toList (treeInsert 3 (fromList [1, 2])) `shouldBe` [1, 2, 3]

        it "fromList" $ do
            fromList [1 :: Int] `shouldBe` Node (1 :: Int) Leaf Leaf
