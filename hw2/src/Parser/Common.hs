module Parser.Common where

import           Parser.Core         (Parser, satisfy)

import           Control.Applicative (Alternative (..))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import           Data.Functor        (void)

type ParserC = Parser Char

-- http://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf

char :: Eq t => t -> Parser t t
char = satisfy . (==)

abParser :: ParserC (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: ParserC ()
abParser_ = void abParser

posInt :: ParserC Integer
posInt = pure read <*> some (satisfy isDigit)

space :: ParserC ()
space = void $ many $ satisfy isSpace

space1 :: ParserC ()
space1 = void $ some $ satisfy isSpace

intPair :: ParserC (Integer, Integer)
intPair = (,) <$> posInt <* space1 <*> posInt

intOrUppercase :: ParserC ()
intOrUppercase = void posInt <|> void (satisfy isUpper)

-- http://www.seas.upenn.edu/~cis194/spring13/hw/11-applicative2.pdf

zeroOrMore :: Alternative f => f a -> f [a]
zeroOrMore = many

oneOrMore :: Alternative f => f a -> f [a]
oneOrMore = some

spaces :: ParserC String
spaces = many $ satisfy isSpace

ident :: ParserC String
ident = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
