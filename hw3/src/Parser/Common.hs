module Parser.Common
    ( Parser
    , space
    , lexeme
    , symbol_
    , identifier
    ) where

import           Control.Applicative        (empty, many)
import           Control.Monad              (void)

import           Data.ByteString            (ByteString, pack)
import           Data.Void                  (Void)

import           Text.Megaparsec            (Parsec, (<?>))
import           Text.Megaparsec.Byte       (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

space :: Parser ()
space = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol_ :: ByteString -> Parser ()
symbol_ = void . L.symbol space

identifier :: Parser ByteString
identifier = lexeme (pack <$> ((:) <$> letterChar <*> many alphaNumChar))
        <?> "identifier"
