{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad            (void)
import           Control.Monad.State.Lazy
import           Data.List                (isSuffixOf)
import           Data.Text.Lazy.Builder   (Builder)
import qualified Data.Text.Lazy.Builder   as T
import qualified Data.Text.Lazy.IO        as T
import           System.FilePath.Find
import           System.Environment       (getArgs)
import           Text.Megaparsec

import           CodeGen
import           Lexer
import           Parser
import           Syntax

processFile :: FilePath -> IO Builder
processFile path = do
  src <- T.readFile path
  case parse parseProgram "<stdin>" src of
    Left err  -> error $ "Unable to parse source file: " ++ show src
    Right ast -> pure
              . mconcat
              . flip evalState (Model 0 0 (dropvm path) "")
              . generate $ ast

bootstrp :: Builder
bootstrp = "" -- "@256\nD=A\n@SP\nM=D\n@Sys.init\n0;JMP\n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Error - no source file specified."
    [path] -> do
      files <- find (depth <? 10) (extension ==? ".vm") path
      outB <- traverse processFile files
      let out = T.toLazyText . mconcat $ bootstrp : outB
      T.writeFile (vm2asm path) out
    _  -> putStrLn "Error - too many command line arguments."

vm2asm :: FilePath -> FilePath
vm2asm path = if isSuffixOf ".vm" path
                then reverse . ("msa" ++) . drop 2 $ reverse path
                else makeFilename path

dropvm :: FilePath -> Builder
dropvm = T.fromString . reverse . takeWhile (/= '/') . drop 3 . reverse

makeFilename :: FilePath -> String
makeFilename path = path ++
                  ( reverse . ("msa." ++) . takeWhile (/= '/') . drop 1 . reverse
                  $ path
                  )
