module Lexer where

import Token
import RegEx
import Thompson
import Automata
import Minimizar
import MDD
import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set

-- ============================================================================
-- Complete Lexer for IMP Language
-- ============================================================================

-- Lexer result: either error or list of tokens
type LexerResult = Either String [Token]

-- Main lexer function
lexer :: String -> LexerResult
lexer input = tokenize impMDD input 0
  where
    tokenize :: MDD -> String -> Int -> LexerResult
    tokenize mdd [] _ = Right []
    tokenize mdd str pos
      -- Skip whitespace
      | not (null str) && isSpace (head str) =
          let (spaces, rest) = span isSpace str
              newPos = pos + length spaces
          in tokenize mdd rest newPos
      
      -- Try to match a token
      | otherwise =
          case maximumMunch mdd str of
            Just (tokenType, len, remaining) ->
              let token = Token tokenType pos
              in case tokenize mdd remaining (pos + len) of
                   Right tokens -> Right (token : tokens)
                   Left err -> Left err
            
            Nothing ->
              Left $ "Lexical error at position " ++ show pos ++ 
                     ": unexpected character '" ++ take 1 str ++ "'"


-- ============================================================================
-- IMP Language Token Specifications
-- ============================================================================

-- Build complete MDD for IMP language
impMDD :: MDD
impMDD = buildMDD
  -- Keywords (highest priority)
  [ (TSkip,  minimizeDFA (string "skip"),   priorityKeyword)
  , (TIf,    minimizeDFA (string "if"),     priorityKeyword)
  , (TThen,  minimizeDFA (string "then"),   priorityKeyword)
  , (TElse,  minimizeDFA (string "else"),   priorityKeyword)
  , (TWhile, minimizeDFA (string "while"),  priorityKeyword)
  , (TDo,    minimizeDFA (string "do"),     priorityKeyword)
  , (TTrue,  minimizeDFA (string "true"),   priorityKeyword)
  , (TFalse, minimizeDFA (string "false"),  priorityKeyword)
  , (TNot,   minimizeDFA (string "not"),    priorityKeyword)
  , (TAnd,   minimizeDFA (string "and"),    priorityKeyword)
  
  -- Multi-character operators
  , (TAssign, minimizeDFA (string ":="),    priorityOperator)
  , (TLeq,    minimizeDFA (string "<="),    priorityOperator)
  
  -- Single-character operators
  , (TPlus,  minimizeDFA (Char '+'),        priorityOperator)
  , (TMinus, minimizeDFA (Char '-'),        priorityOperator)
  , (TTimes, minimizeDFA (Char '*'),        priorityOperator)
  , (TEq,    minimizeDFA (Char '='),        priorityOperator)
  
  -- Delimiters
  , (TSemi,   minimizeDFA (Char ';'),       priorityDelimiter)
  , (TLParen, minimizeDFA (Char '('),       priorityDelimiter)
  , (TRParen, minimizeDFA (Char ')'),       priorityDelimiter)
  
  -- Identifiers: [a-z][a-z0-9]* (lower priority than keywords)
  , (TId "", minimizeDFA identifierRegex,   priorityIdentifier)
  
  -- Numbers: [0-9]+
  , (TNum 0, minimizeDFA numberRegex,       priorityNumber)
  ]


-- Regular expressions for identifiers and numbers
identifierRegex :: RegEx
identifierRegex = Concat lowercase (Star (Union lowercase digit))

numberRegex :: RegEx
numberRegex = Plus digit


-- Helper: Convert RegEx to minimized DFAInt
minimizeDFA :: RegEx -> DFAInt
minimizeDFA regex = 
  let nfae = thompson regex
      nfa = removeEpsilon nfae
      dfa = subsetConstruction nfa
      dfaInt = dfaToInt dfa
  in minimize dfaInt


-- ============================================================================
-- Enhanced Lexer with Token Text Extraction
-- ============================================================================

-- Lexer that also captures the actual text of tokens (for identifiers/numbers)
lexerWithText :: String -> Either String [Token]
lexerWithText input = tokenizeWithText impMDD input 0
  where
    tokenizeWithText :: MDD -> String -> Int -> Either String [Token]
    tokenizeWithText mdd [] _ = Right []
    tokenizeWithText mdd str pos
      -- Skip whitespace
      | not (null str) && isSpace (head str) =
          let (spaces, rest) = span isSpace str
              newPos = pos + length spaces
          in tokenizeWithText mdd rest newPos
      
      -- Try to match a token
      | otherwise =
          case maximumMunch mdd str of
            Just (tokenType, len, remaining) ->
              let tokenText = take len str
                  -- Enhance token with actual text for ID and NUM
                  enhancedToken = case tokenType of
                    TId _ -> Token (TId tokenText) pos
                    TNum _ -> Token (TNum (read tokenText :: Int)) pos
                    _ -> Token tokenType pos
              in case tokenizeWithText mdd remaining (pos + len) of
                   Right tokens -> Right (enhancedToken : tokens)
                   Left err -> Left err
            
            Nothing ->
              Left $ "Lexical error at position " ++ show pos ++ 
                     ": unexpected character '" ++ take 1 str ++ "'"


-- ============================================================================
-- Convenience Functions
-- ============================================================================

-- Tokenize and return just token types (ignoring position)
tokenTypes :: String -> Either String [TokenType]
tokenTypes input = case lexerWithText input of
  Right tokens -> Right (map tokenType tokens)
  Left err -> Left err


-- Tokenize and pretty print
prettyTokens :: String -> String
prettyTokens input = case lexerWithText input of
  Right tokens -> unlines [show i ++ ": " ++ show tok | (i, tok) <- zip [1..] tokens]
  Left err -> "ERROR: " ++ err


-- Check if input is lexically valid
isValid :: String -> Bool
isValid input = case lexer input of
  Right _ -> True
  Left _ -> False


-- ============================================================================
-- Test Cases for IMP
-- ============================================================================

-- Test: simple assignment
test1 :: String
test1 = "x := 5"

-- Test: arithmetic expression
test2 :: String
test2 = "y := x + 3 * 2"

-- Test: conditional
test3 :: String
test3 = "if x <= 10 then skip else y := 1"

-- Test: while loop
test4 :: String
test4 = "while x <= 100 do x := x + 1"

-- Test: complete program
test5 :: String
test5 = "x := 0; while x <= 5 do x := x + 1"

-- Test: boolean expressions
test6 :: String
test6 = "if true and not false then skip else skip"

-- Test: complex program
test7 :: String
test7 = unlines
  [ "x := 0;"
  , "y := 1;"
  , "while x <= 10 do"
  , "  y := y * 2"
  ]


-- Run all tests
runTests :: IO ()
runTests = do
  putStrLn "=== IMP Lexer Tests ===\n"
  
  putStrLn "Test 1: Simple assignment"
  putStrLn test1
  putStrLn $ prettyTokens test1
  
  putStrLn "\nTest 2: Arithmetic expression"
  putStrLn test2
  putStrLn $ prettyTokens test2
  
  putStrLn "\nTest 3: Conditional"
  putStrLn test3
  putStrLn $ prettyTokens test3
  
  putStrLn "\nTest 4: While loop"
  putStrLn test4
  putStrLn $ prettyTokens test4
  
  putStrLn "\nTest 5: Complete program"
  putStrLn test5
  putStrLn $ prettyTokens test5
  
  putStrLn "\nTest 6: Boolean expressions"
  putStrLn test6
  putStrLn $ prettyTokens test6
  
  putStrLn "\nTest 7: Complex program"
  putStrLn test7
  putStrLn $ prettyTokens test7


-- ============================================================================
-- Statistics and Analysis
-- ============================================================================

-- Count tokens in input
tokenCount :: String -> Either String Int
tokenCount input = case lexer input of
  Right tokens -> Right (length tokens)
  Left err -> Left err


-- Get token type distribution
tokenDistribution :: String -> Either String [(TokenType, Int)]
tokenDistribution input = case tokenTypes input of
  Right types -> 
    let count t = length (filter (== t) types)
        unique = Set.toList (Set.fromList types)
    in Right [(t, count t) | t <- unique]
  Left err -> Left err


-- Lexer statistics
lexerStats :: String -> String
lexerStats input = case lexerWithText input of
  Right tokens ->
    unlines
      [ "Total tokens: " ++ show (length tokens)
      , "Token types: " ++ show (Set.size $ Set.fromList $ map tokenType tokens)
      , "Input length: " ++ show (length input)
      ]
  Left err -> "ERROR: " ++ err
