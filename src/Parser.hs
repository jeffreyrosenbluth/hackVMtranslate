{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Text.Lazy            (Text)
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Text.Lazy (Parser)

import           Lexer
import           Syntax

command :: Parser Command
command = CArithmetic <$> arithmetic
      <|> CMemory <$> memory
      -- <|> CFlow <$> flow
      -- <|> CCalling <$> calling

arithmetic :: Parser Arithmetic
arithmetic = try (Add <$ symbol "add")
         <|> Sub <$ symbol "sub"
         <|> try (Neg <$ symbol "neg")
         <|> Eq <$ symbol "eq"
         <|> Gt <$ symbol "gt"
         <|> Lt <$ symbol "lt"
         <|> And <$ symbol "and"
         <|> Or <$ symbol "or"
         <|> Not <$ symbol "not"

memory :: Parser Memory
memory = try (Push <$> (symbol "push" *> segment) <*> (fromInteger <$> integer))
     <|> Pop <$> (symbol "pop" *> segment) <*> (fromInteger <$> integer)

segment :: Parser Segment
segment = Argument <$ symbol "argument"
      <|> Local <$ symbol "local"
      <|> Static <$ symbol "static"
      <|> Constant <$ symbol "constant"
      <|> try (This <$ symbol "this")
      <|> try (That <$ symbol "that")
      <|> Pointer <$ symbol "pointer"
      <|> Temp <$ symbol "temp"


flow :: Parser Flow
flow = undefined

calling :: Parser Calling
calling = undefined

parseProgram :: Parser [Command]
parseProgram =contents $ some command
