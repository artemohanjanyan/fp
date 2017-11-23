module Parser.Common
    ( Parser
    , space
    , lexeme
    , symbol_
    ) where

import           Control.Applicative        (empty)
import           Control.Monad              (void)

import           Data.ByteString            (ByteString)
import           Data.Void                  (Void)

import           Text.Megaparsec            (Parsec)
import           Text.Megaparsec.Byte       (space1)
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

space :: Parser ()
space = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol_ :: ByteString -> Parser ()
symbol_ = void . L.symbol space
