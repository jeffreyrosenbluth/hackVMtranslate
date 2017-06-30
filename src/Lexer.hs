module Lexer  where

import           Control.Applicative
import           Control.Monad             (void)
import           Data.Char
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer     as L
import           Text.Megaparsec.Text.Lazy

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt empty
  where lineCmnt  = L.skipLineComment "//"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.integer

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

contents :: Parser a -> Parser a
contents p = sc *> p <* eof

isHackSymbol :: Char -> Bool
isHackSymbol c = isAlphaNum c || c `elem` "_.$:"

isStartSymbol :: Char -> Bool
isStartSymbol c = isAlpha c || c `elem` "_.$:"

identifier :: Parser String
identifier = lexeme  . try $ (:)
         <$> satisfy isStartSymbol
         <*> many (satisfy isHackSymbol)
