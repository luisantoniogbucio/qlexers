module Main where

import Lexer
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      -- Interactive mode
      putStrLn "=== IMP Lexer ==="
      putStrLn "Running test suite...\n"
      runTests
    
    ["--file", filename] -> do
      -- File mode
      content <- readFile filename
      putStrLn $ "Tokenizing file: " ++ filename
      putStrLn $ prettyTokens content
    
    ["--test", code] -> do
      -- Single test mode
      putStrLn $ prettyTokens code
    
    _ -> do
      putStrLn "Usage:"
      putStrLn "  qlexers                    -- Run test suite"
      putStrLn "  qlexers --file <file>      -- Tokenize file"
      putStrLn "  qlexers --test \"<code>\"    -- Tokenize code string"
