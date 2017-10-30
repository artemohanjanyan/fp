module Parser.Lets
    ( Let (..)
    , simplify
    , printLet
    ) where

import           Parser.Common       (ParserC, char, ident, posInt, sepBy, space, space1)
import           Parser.Core         (eof)

import           Control.Applicative (Alternative (..))
import           Control.Monad       (forM_, guard)
import qualified Data.Map.Strict     as Map

newtype Let = Let [(String, Integer)]
    deriving (Show, Eq)

simplify :: Map.Map String Integer -> ParserC Let
simplify scope =
    (eof *> pure (Let [])) <|>

    (space >>
    char 'l' >>
    char 'e' >>
    char 't' >>
    space1 >>
    ident >>= \varName ->
    guard (not $ Map.member varName scope) >>
    space >>
    char '=' >>
    space >>

    let scopedVar = ident >>= \vn -> case Map.lookup vn scope of
            Just value -> pure value
            Nothing    -> empty in
    let parseNum = posInt <|> scopedVar in

    sepBy parseNum (space >> char '+' >> space) >>= \numbers ->

    let varValue = sum numbers in
    let newScope = Map.insert varName varValue scope in

    space >>
    simplify newScope >>= \(Let rest) ->
    pure (Let ((varName, varValue) : rest)))

printLet :: Let -> IO ()
printLet (Let exprs) = forM_ exprs printPair
  where
    printPair (varName, varValue) =
        putStr "let " >>
        putStr varName >>
        putStr " = " >>
        print varValue
