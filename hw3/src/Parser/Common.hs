module Parser.Common
    ( Parser
    , space
    , lexeme
    , symbol_
    , identifier
    ) where

import           Control.Applicative        (empty)
import           Control.Monad              (void)

import           Data.ByteString            (ByteString, cons)
import           Data.Char                  (chr, isAlphaNum)
import           Data.Void                  (Void)

import           Text.Megaparsec            (Parsec, takeWhileP, (<?>))
import           Text.Megaparsec.Byte       (letterChar, space1)
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

space :: Parser ()
space = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol_ :: ByteString -> Parser ()
symbol_ = void . L.symbol space

identifier :: Parser ByteString
identifier = lexeme (cons <$> letterChar <*> takeWhileP Nothing isWord8AlphaNum)
        <?> "identifier"
  where
    isWord8AlphaNum = isAlphaNum . chr . fromIntegral
