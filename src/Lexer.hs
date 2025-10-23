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
import Data.Bifunctor (first)
import Control.Arrow ((>>>))
import Control.Monad (guard)

-- lexer como morfismo: String -> Either Error [Token]
type LexerResult = Either String [Token]

-- scanner principal: entrada -> salida tokenizada
lexer :: String -> LexerResult
lexer = scan impMDD 0
  where
    scan :: MDD -> Int -> String -> LexerResult
    scan _ _ [] = Right []
    scan mdd pos str
      | isSpace (head str) = scan mdd (pos + n) rest
      | otherwise = do
          (tok, len, rest') <- maybeToEither lexError $ maximumMunch mdd str
          let token = Token tok pos
          tokens <- scan mdd (pos + len) rest'
          return (token : tokens)
      where
        (spaces, rest) = span isSpace str
        n = length spaces
        lexError = "lexical error at position " ++ show pos ++ 
                   ": unexpected '" ++ take 1 str ++ "'"

-- mdd del lenguaje imp: union de todos los patrones
impMDD :: MDD
impMDD = buildMDD $ concat
  [ keywords
  , multiCharOps
  , singleCharOps
  , delimiters
  , [ (TId "", minimizeRE identifierRegex, priorityIdentifier)
    , (TNum 0, minimizeRE numberRegex, priorityNumber)
    ]
  ]

-- especificacion lexica por categorias
keywords :: [(TokenType, DFAInt, Int)]
keywords = 
  [ (TSkip,  "skip")
  , (TIf,    "if")
  , (TThen,  "then")
  , (TElse,  "else")
  , (TWhile, "while")
  , (TDo,    "do")
  , (TTrue,  "true")
  , (TFalse, "false")
  , (TNot,   "not")
  , (TAnd,   "and")
  ] >>= \(tok, str) -> return (tok, minimizeRE (string str), priorityKeyword)

multiCharOps :: [(TokenType, DFAInt, Int)]
multiCharOps =
  [ (TAssign, ":=")
  , (TLeq,    "<=")
  ] >>= \(tok, str) -> return (tok, minimizeRE (string str), priorityOperator)

singleCharOps :: [(TokenType, DFAInt, Int)]
singleCharOps =
  [ (TPlus,  '+')
  , (TMinus, '-')
  , (TTimes, '*')
  , (TEq,    '=')
  ] >>= \(tok, c) -> return (tok, minimizeRE (Char c), priorityOperator)

delimiters :: [(TokenType, DFAInt, Int)]
delimiters =
  [ (TSemi,   ';')
  , (TLParen, '(')
  , (TRParen, ')')
  ] >>= \(tok, c) -> return (tok, minimizeRE (Char c), priorityDelimiter)

-- expresiones regulares de componentes lexicos
identifierRegex :: RegEx
identifierRegex = Concat lowercase (Star (Union lowercase digit))

numberRegex :: RegEx
numberRegex = Plus digit

-- pipeline de compilacion: RE -> DFAInt minimizado
minimizeRE :: RegEx -> DFAInt
minimizeRE = thompson >>> removeEpsilon >>> subsetConstruction >>> dfaToInt >>> minimize

-- lexer con extraccion de atributos (valor del token)
lexerWithText :: String -> LexerResult
lexerWithText = scanWithText impMDD 0
  where
    scanWithText :: MDD -> Int -> String -> LexerResult
    scanWithText _ _ [] = Right []
    scanWithText mdd pos str
      | isSpace (head str) = scanWithText mdd (pos + n) rest
      | otherwise = do
          (tok, len, rest') <- maybeToEither lexError $ maximumMunch mdd str
          let lexeme = take len str
              token = enhanceToken tok lexeme pos
          tokens <- scanWithText mdd (pos + len) rest'
          return (token : tokens)
      where
        (spaces, rest) = span isSpace str
        n = length spaces
        lexError = "lexical error at position " ++ show pos ++ 
                   ": unexpected '" ++ take 1 str ++ "'"

-- enhancer: asigna atributo al token (id/num llevan lexema)
enhanceToken :: TokenType -> String -> Int -> Token
enhanceToken (TId _) lexeme pos = Token (TId lexeme) pos
enhanceToken (TNum _) lexeme pos = Token (TNum (read lexeme)) pos
enhanceToken tok _ pos = Token tok pos

-- utilidad: Maybe -> Either con mensaje
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

-- proyeccion: tokens -> tipos
tokenTypes :: String -> Either String [TokenType]
tokenTypes = lexerWithText >>> fmap (map tokenType)

-- pretty printer
prettyTokens :: String -> String
prettyTokens input = case lexerWithText input of
  Right tokens -> unlines [show i ++ ": " ++ show tok | (i, tok) <- zip [1..] tokens]
  Left err -> "ERROR: " ++ err

-- predicado de validez
isValid :: String -> Bool
isValid = lexer >>> either (const False) (const True)

-- casos de prueba
test1, test2, test3, test4, test5, test6, test7 :: String
test1 = "x := 5"
test2 = "y := x + 3 * 2"
test3 = "if x <= 10 then skip else y := 1"
test4 = "while x <= 100 do x := x + 1"
test5 = "x := 0; while x <= 5 do x := x + 1"
test6 = "if true and not false then skip else skip"
test7 = unlines
  [ "x := 0;"
  , "y := 1;"
  , "while x <= 10 do"
  , "  y := y * 2"
  ]

-- suite de pruebas
runTests :: IO ()
runTests = do
  let tests = [("Simple assignment", test1)
              ,("Arithmetic expression", test2)
              ,("Conditional", test3)
              ,("While loop", test4)
              ,("Complete program", test5)
              ,("Boolean expressions", test6)
              ,("Complex program", test7)]
  
  putStrLn "=== IMP Lexer Tests ===\n"
  mapM_ runTest tests
  where
    runTest (name, input) = do
      putStrLn $ "Test: " ++ name
      putStrLn input
      putStrLn $ prettyTokens input
      putStrLn ""

-- estadisticas: conteo y distribucion
tokenCount :: String -> Either String Int
tokenCount = lexer >>> fmap length

tokenDistribution :: String -> Either String [(TokenType, Int)]
tokenDistribution input = do
  types <- tokenTypes input
  let unique = Set.toList . Set.fromList $ types
      count t = length (filter (== t) types)
  return [(t, count t) | t <- unique]

lexerStats :: String -> String
lexerStats input = case lexerWithText input of
  Right tokens -> unlines
    [ "Total tokens: " ++ show (length tokens)
    , "Token types: " ++ show (Set.size . Set.fromList . map tokenType $ tokens)
    , "Input length: " ++ show (length input)
    ]
  Left err -> "ERROR: " ++ err
