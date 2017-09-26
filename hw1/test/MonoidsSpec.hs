module MonoidsSpec where

import Monoids
import Adts

import Data.Semigroup (Semigroup (..))
import Data.Monoid (Sum (..))

import Test.Hspec

spec :: Spec
spec = do
    it "maybeConcat" $
        maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1..5]

    it "eitherConcat" $
        eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
            `shouldBe` (Sum {getSum = 8}, [1,2,3,4,5])

    it "NonEmpty Semigroup" $ do
        (1 :| [2, 3, 4]) <> (5 :| [])     `shouldBe` (1 :| [2..5])
        (1 :| [2])       <> (3 :| [4, 5]) `shouldBe` (1 :| [2..5])

    it "Identity" $ do
        Identity [1, 2, 3] <> Identity [4, 5]
            `shouldBe` Identity ([1, 2, 3] <> [4, 5])
        Identity (mempty :: [Int]) `shouldBe` (mempty :: Identity [Int])
        runIdentity (mappend (Identity [1, 2, 3]) (Identity [4, 5]))
            `shouldBe` mappend [1, 2, 3] [4, 5]

    it "Name" $ do
        (Name "root" <> Name "server") `shouldBe` Name "root.server"
        (Name "root" <> mempty)        `shouldBe` (mempty <> Name "root")

    it "Endo" $ do
        getEndo (mempty <> Endo (+1) ) 1 `shouldBe`
            getEndo (Endo (+1) <> mempty) 1
        getEndo ((Endo (+1) <> Endo (+2)) <> Endo (+3)) 4 `shouldBe`
            getEndo (Endo (+1) <> (Endo (+2) <> Endo (+3))) 4

    it "Arrow" $ do
        let mkArrow = Arrow . fmap . (+)
        getArrow (mempty `mappend` mkArrow 1) (Sum 1) `shouldBe`
            getArrow (mkArrow 1 `mappend` mempty) (Sum 1)
        getArrow ((mkArrow 1 <> mkArrow 2) <> mkArrow 3) (Sum 4) `shouldBe`
            getArrow (mkArrow 1 <> (mkArrow 2 <> mkArrow 3)) (Sum 4)

    it "Monoid Tree" $ do
        fromList [1, 2, 3] <> mempty `shouldBe`
            mempty <> fromList [1, 2, 3]
        (fromList [1, 2] <> fromList [3, 4]) <> fromList [5, 6] `shouldBe`
            fromList [1, 2] <> (fromList [3, 4] <> fromList [5, 6])
