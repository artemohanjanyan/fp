module Parsers where

import           Control.Applicative (Alternative (..))
import           Control.Monad       ((>=>))
import           Data.Char           (isDigit, isSpace, isUpper)
import           Data.Functor        (void)
import           Data.Semigroup      (Semigroup (..))
--import           Data.Bifunctor (Bifunctor (..))

data ParseError = ParseError
    deriving (Show)

-- Monstupar
newtype Parser t a = Parser
    { runParser :: [t] -> Either ParseError ([t], a) }

type ParserC = Parser Char

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
    []    -> Left ParseError
    t:ts' | p t -> Right (ts', t)
    _     -> Left ParseError

char :: Eq t => t -> Parser t t
char = satisfy . (==)

instance Functor (Parser t) where
    fmap f p = Parser $ fmap (fmap f) . runParser p

instance Applicative (Parser t) where
    pure x = Parser $ \ts -> Right (ts, x)
    p1 <*> p2 = Parser $
        runParser p1     >=> \(ts',  f) ->
        runParser p2 ts' >>= \(ts'', x) ->
        pure (ts'', f x)

instance Alternative (Parser t) where
    empty = Parser $ const $ Left ParseError
    x <|> y = Parser $ \ts -> runParser x ts <> runParser y ts
    --x <|> y = Parser $ liftA2 (<>) (runParser x) (runParser y)

-- http://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf

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
