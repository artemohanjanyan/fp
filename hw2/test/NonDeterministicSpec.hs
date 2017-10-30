module NonDeterministicSpec where

import           NonDeterministic

import           Test.Hspec

spec :: Spec
spec = do
    it "bin" $ do
        bin 1 `shouldMatchList` [[0], [1]]
        bin 2 `shouldMatchList` [[0, 0], [1, 0], [0, 1], [1, 1]]
        bin 3 `shouldMatchList`
                [ [ 0 , 0 , 0 ]
                , [ 1 , 0 , 0 ]
                , [ 0 , 1 , 0 ]
                , [ 1 , 1 , 0 ]
                , [ 0 , 0 , 1 ]
                , [ 1 , 0 , 1 ]
                , [ 0 , 1 , 1 ]
                , [ 1 , 1 , 1 ]
                ]

    it "combinations" $ do
        combinations 4 2 `shouldMatchList`
                [ [2, 1]
                , [3, 1]
                , [4, 1]
                , [3, 2]
                , [4, 2]
                , [4, 3]
                ]

    it "permutations" $ do
        permutations [22, 10, 5] `shouldMatchList`
                [ [22, 10, 5]
                , [22, 5, 10]
                , [10, 22, 5]
                , [10, 5, 22]
                , [5, 22, 10]
                , [5, 10, 22]
                ]
