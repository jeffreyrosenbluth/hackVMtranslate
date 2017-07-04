module Syntax where

import           Data.Word

data Command
  = CArithmetic Arithmetic
  | CMemory Memory
  | CFlow Flow
  | CCalling Calling
    deriving Show

data Arithmetic
  = Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  deriving Show

data Memory
  = Push Segment Word16
  | Pop  Segment Word16
  deriving Show

data Segment
  = Argument
  | Local
  | Static
  | Constant
  | This
  | That
  | Pointer
  | Temp
  deriving (Show, Eq)

data Flow
  = Label String
  | Goto String
  | IfGoto String
  deriving Show

data Calling
  = Function String Word16
  | Call String Word16
  | Return
  deriving Show
