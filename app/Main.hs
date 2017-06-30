{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad            (void)
import           Control.Monad.State.Lazy
import           Data.List                (isSuffixOf)
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.IO        as T
import           System.Directory
import           System.Environment       (getArgs)
import           Text.Megaparsec
import           Text.Printf

import           CodeGen
import           Lexer
import           Parser
import           Syntax

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Error - no source file specified."
    [path] -> do
      fileExists <- doesFileExist path
      dirExists <- doesDirectoryExist path
      let correctExtension = isSuffixOf ".vm" path
      if fileExists
        then do
          src <- T.readFile path
          case parse parseProgram "<stdin>" src of
            Left err -> print err
            Right ast -> do
              let commands = flip evalState (Model 0 (dropvm path)) . generate $ ast
                  out = T.unpack $ T.concat commands
              writeFile (vm2asm path) out
        else putStrLn "Error - source file does not exist."
    _  -> putStrLn "Error - too many command line arguments."

-- | Add an extension to a 'FilePath' if it does not alread have one.
withExt :: FilePath -> FilePath -> FilePath
withExt ext base
  | '.' `elem` base = base
  | otherwise       = base ++ "." ++ ext

vm2asm :: FilePath -> FilePath
vm2asm = reverse . ("msa" ++) . drop 2 . reverse . withExt "vm"

dropvm :: FilePath -> Text
dropvm = T.pack . reverse . takeWhile (/= '/') . drop 3 . reverse
