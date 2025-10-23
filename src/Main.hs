module Main where

import Lexer (runTests, prettyTokens)
import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad ((>=>))

-- modos de ejecucion: interactivo | archivo | codigo
data Mode = Interactive | File FilePath | Code String

-- parser de argumentos: [String] -> Maybe Mode
parseArgs :: [String] -> Maybe Mode
parseArgs [] = Just Interactive
parseArgs ["--file", path] = Just (File path)
parseArgs ["--test", code] = Just (Code code)
parseArgs _ = Nothing

-- ejecutar modo: Mode -> IO ()
runMode :: Mode -> IO ()
runMode Interactive = do
  putStrLn "=== IMP Lexer ==="
  putStrLn "Running test suite...\n"
  runTests

runMode (File path) = do
  content <- readFile path
  putStrLn $ "Tokenizing: " ++ path
  putStrLn $ prettyTokens content

runMode (Code code) = putStrLn $ prettyTokens code

-- mensaje de uso
usage :: String
usage = unlines
  [ "Usage:"
  , "  qlexers                    -- run test suite"
  , "  qlexers --file <file>      -- tokenize file"
  , "  qlexers --test \"<code>\"    -- tokenize code string"
  ]

-- punto de entrada: composicion de parser y ejecutor
main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just mode -> runMode mode
    Nothing -> putStrLn usage  
