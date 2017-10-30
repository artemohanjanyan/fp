module Parser.SExprSpec where

import           Parser.Core
import           Parser.SExpr

import           Test.Hspec

spec :: Spec
spec = it "parseSExpr" $ do
    runParser parseSExpr "5" `shouldBe` Right ("",SAtom (N 5))
    runParser parseSExpr "foo3" `shouldBe`
            Right ("",SAtom (I (Ident "foo3")))
    runParser parseSExpr "(bar (foo) 3 5 874)" `shouldBe`
            Right ("", Comb [ SAtom (I (Ident "bar"))
                            , Comb [SAtom (I (Ident "foo"))]
                            , SAtom (N 3)
                            , SAtom (N 5)
                            , SAtom (N 874)
                            ])
