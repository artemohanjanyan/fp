module Parsers where

data ParseError = ParseError
    deriving (Show)

-- Monstupar
newtype Parser s a = Parser
    { runParser :: [s] -> Either ParseError ([s], a) }

satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ \ts -> case ts of
    []    -> Left ParseError
    t:ts' | p t -> Right (ts', t)
    _     -> Left ParseError
