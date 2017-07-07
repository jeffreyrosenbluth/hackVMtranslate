{-# LANGUAGE OverloadedStrings #-}

module CodeGen where

import           Control.Monad.State.Lazy
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Data.Word
import           TextShow

import           Syntax

data Model = Model
  { labelID :: Word16
  , returnID :: Word16
  , moduleName :: Builder
  , functionName :: Builder
  } deriving Show

type Generator = State Model Builder

incLabelID  :: State Model ()
incLabelID = modify (\model -> model {labelID = labelID model + 1})

incReturnID  :: State Model ()
incReturnID = modify (\model -> model {returnID = returnID model + 1})

data Reg = A | D deriving Show

showReg :: Reg -> Text
showReg A = "A"
showReg D = "D"

generate :: [Command] -> State Model [Builder]
generate = traverse go
  where
    go (CMemory (Push s i))      = push s i
    go (CMemory (Pop s i))       = pop s i
    go (CArithmetic Add)         = binop "+"
    go (CArithmetic Sub)         = binop "-"
    go (CArithmetic Neg)         = unary "-"
    go (CArithmetic Eq)          = comp "JEQ"
    go (CArithmetic Gt)          = comp "JGT"
    go (CArithmetic Lt)          = comp "JLT"
    go (CArithmetic And)         = binop "&"
    go (CArithmetic Or)          = binop "|"
    go (CArithmetic Not)         = unary "!"
    go (CFlow (Label l))         = label l
    go (CFlow (Goto l))          = goto l
    go (CFlow (IfGoto l))        = gotoIf l
    go (CCalling (Function s n)) = function s n
    go (CCalling (Call s n))     = call s n
    go (CCalling Return)         = ret

binop :: Builder -> Generator
binop op = pure $ "@SP\nAM=M-1\nD=M\nA=A-1\nM=M" <> op <> "D\n"

unary :: Builder -> Generator
unary op = pure $ "@SP\nA=M-1\nM=" <> op <> "M\n"

comp :: Builder -> Generator
comp jmp = do
  incLabelID
  tr <- gets labelID
  incLabelID
  cont <- gets labelID
  pure $ "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@LABEL." <> showb tr <> "\n"
      <> "D;" <> jmp <> "\n"
      <> "@SP\nA=M-1\nM=0\n@LABEL." <> showb cont <> "\n"
      <> "0;JMP\n(LABEL." <> showb tr <> ")\n"
      <> "@SP\nA=M-1\nM=-1\n(LABEL." <> showb cont <> ")\n"

push :: Segment -> Word16 -> Generator
push seg idx
  | seg == Constant
      =  pure $ "// Push Constant " <> showb idx <> "\n"
      <> "@" <> showb idx
      <> "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
  | seg == Temp || seg == Pointer
      =  pure $ "// Push " <> base seg <> " " <> showb idx <> "\n"
      <> "@" <> base seg
      <> "\nD=A\n@" <> showb idx
      <> "\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
  | seg == Static = do
      name <- gets moduleName
      let label = name <> "." <> showb idx
      pure $ "// Push STATIC "  <> showb idx <> "\n"
          <> "@" <> label
          <> "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
  | otherwise
      =  pure $ "// Push " <> base seg <> " " <> showb idx <> "\n"
      <> "@" <> base seg
      <> "\nD=M\n@" <> showb idx
      <> "\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"

pop :: Segment -> Word16 -> Generator
pop seg idx
  | seg == Temp || seg == Pointer
      = pure $ "// Pop " <> base seg <> " " <> showb idx <> "\n"
      <>  "@" <> base seg
      <> "\nD=A\n@" <> showb idx <> "\nD=D+A\n"
      <> "@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D\n"
  | seg == Static = do
      name <- gets moduleName
      let label = name <> "." <> showb idx
      pure $ "// Pop STATIC " <> showb idx <> "\n"
        <>  "@" <> label
        <> "\nD=A\n"
        <> "@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D\n"
  | otherwise
      =  pure $ "// Pop " <> base seg <> " " <> showb idx <> "\n"
      <>  "@" <> base seg
      <> "\nD=M\n@" <> showb idx <> "\nD=D+A\n"
      <> "@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D\n"

base :: Segment -> Builder
base Argument = "ARG"
base Local    = "LCL"
base Static   = error "Static segments do not have a base."
base Constant = error "Constant segments do not have a base."
base This     = "THIS"
base That     = "THAT"
base Pointer  = "R3"
base Temp     = "R5"

label :: String -> Generator
label s = do
  fName <- gets functionName
  pure $ "(" <> fName <> "$" <> T.fromString s <> ")\n"

goto :: String -> Generator
goto s = do
  fName <- gets functionName
  pure $ "@" <> fName <> "$" <> T.fromString s <> "\n0;JMP\n"

gotoIf :: String -> Generator
gotoIf s = do
  fName <- gets functionName
  pure $ "// GotoIF\n@SP\nAM=M-1\nD=M\n@" <> fName <> "$" <> T.fromString s
      <> "\nD;JNE\n"

function :: String -> Word16 -> Generator
function s k = do
  modify (\model -> model {functionName = T.fromString s})
  lVar <- push Constant 0
  let xs = mconcat $ replicate (fromIntegral k) lVar
  pure $ "(" <> T.fromString s <> ")\n" <> xs

call :: String -> Word16 -> Generator
call s n = do
  incReturnID
  r <- gets returnID
  m <- gets moduleName
  let rtn = "RETURN." <> m <> "." <> showb r
  pure $ "@" <> rtn <> "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
      <> psh "LCL"
      <> psh "ARG"
      <> psh "THIS"
      <> psh "THAT"
      <> "@SP\nD=M\n@" <> showb (n + 5) <> "\nD=D-A\n@ARG\nM=D\n"
      <> "@SP\nD=M\n@LCL\nM=D\n"
      <> "@" <> T.fromString s <> "\n0;JMP\n"
      <> "(" <> rtn <> ")\n"
  where
    psh l = "@" <> l <> "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"

ret :: Generator
ret = pure $ "// Return\n@LCL\nD=M\n@R13\nM=D\n"
          <> "@R13\nD=M\n@5\nA=D-A\nD=M\n@R14\nM=D\n"
          <> "@SP\nAM=M-1\nD=M\n@ARG\nA=M\nM=D\n"
          <> "@ARG\nD=M+1\n@SP\nM=D\n"
          <> "@R13\nA=M-1\nD=M\n@THAT\nM=D\n"
          <> "@R13\nD=M\n@2\nA=D-A\nD=M\n@THIS\nM=D\n"
          <> "@R13\nD=M\n@3\nA=D-A\nD=M\n@ARG\nM=D\n"
          <> "@R13\nD=M\n@4\nA=D-A\nD=M\n@LCL\nM=D\n"
          <> "@R14\nA=M\n0;JMP\n"
