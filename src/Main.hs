module Main where

import Lexer
import LexerSQL
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "=== Lexers IMP y SQL ==="
      putStrLn "Uso:"
      putStrLn "  qlexers --imp --archivo <archivo>    -- Tokenizar archivo IMP"
      putStrLn "  qlexers --imp --codigo \"<código>\"    -- Tokenizar código IMP"
      putStrLn "  qlexers --sql --archivo <archivo>    -- Tokenizar archivo SQL"
      putStrLn "  qlexers --sql --codigo \"<código>\"    -- Tokenizar código SQL"
    
    ["--imp", "--archivo", filename] -> do
      contenido <- readFile filename
      putStrLn $ "Tokenizando archivo IMP: " ++ filename
      let tokens = lexer contenido
      putStrLn $ "Tokens: " ++ show tokens
    
    ["--imp", "--codigo", codigo] -> do
      putStrLn $ "Tokenizando IMP: " ++ codigo
      let tokens = lexer codigo
      putStrLn $ "Tokens: " ++ show tokens
      
    ["--sql", "--archivo", filename] -> do
      contenido <- readFile filename
      putStrLn $ "Tokenizando archivo SQL: " ++ filename
      let tokens = lexerSQL contenido
      putStrLn $ "Tokens: " ++ show tokens
    
    ["--sql", "--codigo", codigo] -> do
      putStrLn $ "Tokenizando SQL: " ++ codigo
      let tokens = lexerSQL codigo
      putStrLn $ "Tokens: " ++ show tokens
    
    _ -> do
      putStrLn "Uso:"
      putStrLn "  qlexers --imp --archivo <archivo>    -- Tokenizar archivo IMP"
      putStrLn "  qlexers --imp --codigo \"<código>\"    -- Tokenizar código IMP"
      putStrLn "  qlexers --sql --archivo <archivo>    -- Tokenizar archivo SQL"
      putStrLn "  qlexers --sql --codigo \"<código>\"    -- Tokenizar código SQL"
