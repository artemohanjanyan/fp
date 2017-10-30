module Parser.Core where

import           Control.Applicative (Alternative (..))
import           Control.Monad       ((>=>))
import           Data.Semigroup      (Semigroup (..))

data ParseError = ParseError
    deriving (Show)

-- Monstupar
newtype Parser t a = Parser
    { runParser :: [t] -> Either ParseError ([t], a) }

eof :: Parser t ()
eof = Parser $ \ts -> case ts of
    [] -> Right ([], ())
    _  -> Left ParseError

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser $ \ts -> case ts of
    []    -> Left ParseError
    t:ts' | p t -> Right (ts', t)
    _     -> Left ParseError

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

instance Monad (Parser t) where
    p >>= f = Parser $ \ts -> case runParser p ts of
        Left e         -> Left e
        Right (ts', a) -> runParser (f a) ts'
