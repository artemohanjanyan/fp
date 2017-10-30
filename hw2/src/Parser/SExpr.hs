module Parser.SExpr
    ( Ident (..)
    , Atom (..)
    , SExpr (..)
    , parseSExpr
    ) where

import           Parser.Common       (ParserC, char, ident, posInt, sepBy, space1)

import           Control.Applicative (Alternative (..))

newtype Ident = Ident String
    deriving (Show, Eq)

data Atom = N Integer | I Ident
    deriving (Show, Eq)

data SExpr
    = SAtom Atom
    | Comb [SExpr]
    deriving (Show, Eq)

parseSExpr :: ParserC SExpr
parseSExpr = (char '(' *> (Comb <$> sepBy parseSExpr space1) <* char ')')
        <|> SAtom <$> parseAtom

parseAtom :: ParserC Atom
parseAtom = N <$> posInt <|> I <$> (Ident <$> ident)
