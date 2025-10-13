module Main where

import Lexer
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "=== Lexer IMP ==="
      putStrLn "Uso: qlexers --archivo <archivo> | --codigo <codigo>"
    
    ["--archivo", filename] -> do
      contenido <- readFile filename
      putStrLn $ "Tokenizando archivo: " ++ filename
      let tokens = lexer contenido
      putStrLn $ "Tokens: " ++ show tokens
    
    ["--codigo", codigo] -> do
      putStrLn $ "Tokenizando: " ++ codigo
      let tokens = lexer codigo
      putStrLn $ "Tokens: " ++ show tokens
    
    _ -> do
      putStrLn "Uso:"
      putStrLn "  qlexers                      -- Ejecutar pruebas"
      putStrLn "  qlexers --archivo <archivo>  -- Tokenizar archivo"
      putStrLn "  qlexers --codigo \"<código>\"  -- Tokenizar código"
