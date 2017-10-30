module Parser.SExpr
    ( Ident
    , Atom (..)
    , SExpr (..)
    , parseSExpr
    , parseAtom
    ) where

import           Parser.Common       (ParserC, char, ident, posInt, sepBy, space1)

import           Control.Applicative (Alternative (..))

newtype Ident = Ident String
    deriving Show

data Atom = N Integer | I Ident
    deriving Show

data SExpr
    = SAtom Atom
    | Comb [SExpr]
    deriving Show

parseSExpr :: ParserC SExpr
parseSExpr = (char '(' *> (Comb <$> sepBy parseSExpr space1) <* char ')')
        <|> SAtom <$> parseAtom

parseAtom :: ParserC Atom
parseAtom = N <$> posInt <|> I <$> (Ident <$> ident)
