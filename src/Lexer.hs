module Lexer where

import Token
import RegExp
import NFA
import DFA
import Minimizar
import MDD
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Define los patrones de tokens para IMP (orden importante: más específicos primero)
patronesIMP :: [(RegExp, Token)]
patronesIMP = 
  [ 
    (cadena ":=", TAssign)
  , (cadena "<=", TLeq)
  , (cadena "skip", TSkip)
  , (cadena "if", TIf)
  , (cadena "then", TThen)
  , (cadena "else", TElse)
  , (cadena "while", TWhile)
  , (cadena "do", TDo)
  , (cadena "true", TTrue)
  , (cadena "false", TFalse)
  , (cadena "not", TNot)
  , (cadena "and", TAnd)
  , (unoOMas digito, TNum 0)
  , (Concat letra (Star (Union letra digito)), TId "")
  , (Char '+', TPlus)
  , (Char '-', TMinus)
  , (Char '*', TTimes)
  , (Char '=', TEq)
  , (Char ';', TSemi)
  , (Char '(', TLParen)
  , (Char ')', TRParen)
  ]

-- Construye el lexer completo para IMP
construirLexerIMP :: [(MDD Token, Token)]
construirLexerIMP = crearLexer patronesIMP

-- Crea un lexer a partir de patrones de tokens
crearLexer :: [(RegExp, Token)] -> [(MDD Token, Token)]
crearLexer patrones = 
  [ (crearMDDParaPatron regex, token) | (regex, token) <- patrones ]
  where
    crearMDDParaPatron regex = 
      let nfa = construirNFA regex
          dfa = nfaADFA nfa
          dfaInt = dfaAEnteros dfa
          dfaMin = minimizarDFA dfaInt
          -- Todos los estados finales mapean al mismo token
          mapeoTokens = Map.fromList 
            [(estado, TId "temp") | estado <- Set.toList (estadosFinalesInt dfaMin)]
      in dfaAMDD dfaMin mapeoTokens

-- Combina múltiples patrones en una expresión regular
combinarPatrones :: [RegExp] -> RegExp
combinarPatrones [] = Empty
combinarPatrones [r] = r
combinarPatrones (r:rs) = Union r (combinarPatrones rs)

-- Crea mapeo de estados finales a tokens
crearMapeoTokens :: DFAInt -> [(RegExp, Token)] -> Map Int Token
crearMapeoTokens dfa patrones = 
  -- Estados finales obtienen el primer token que coincida
  Map.fromList $ zip (Set.toList (estadosFinalesInt dfa)) (map snd patrones)

-- Tokeniza código IMP
tokenizarIMP :: String -> [Token]
tokenizarIMP = tokenizarConLexers construirLexerIMP

-- Tokeniza usando múltiples MDDs
tokenizarConLexers :: [(MDD Token, Token)] -> String -> [Token]
tokenizarConLexers lexers entrada = tokenizarAux [] (saltarEspacios entrada)
  where
    tokenizarAux tokens [] = reverse tokens
    tokenizarAux tokens input =
      case encontrarMejorCoincidencia lexers input of
        Just (token, restante) -> tokenizarAux (token:tokens) (saltarEspacios restante)
        Nothing -> error $ "Carácter no reconocido: " ++ take 1 input

-- Salta espacios en blanco
saltarEspacios :: String -> String
saltarEspacios = dropWhile (`elem` " \t\n\r")

-- Encuentra la mejor coincidencia (maximal munch - más larga)
encontrarMejorCoincidencia :: [(MDD Token, Token)] -> String -> Maybe (Token, String)
encontrarMejorCoincidencia lexers input =
  let coincidencias = [ (token, resto, texto, length texto) 
                      | (mdd, token) <- lexers
                      , Just (_, resto, texto) <- [ejecutarMDD mdd input]
                      ]
      -- Tomar la coincidencia más larga (maximal munch)
      mejores = if null coincidencias 
               then []
               else let maxLen = maximum (map (\(_, _, _, len) -> len) coincidencias)
                    in filter (\(_, _, _, len) -> len == maxLen) coincidencias
  in case mejores of
       ((token, resto, texto, _):_) -> Just (procesarToken token texto, resto)
       [] -> Nothing

-- Procesa el token con el texto capturado
procesarToken :: Token -> String -> Token
procesarToken (TId _) texto = TId texto
procesarToken (TNum _) texto = TNum (read texto)
procesarToken token _ = token

-- Función principal de tokenización
lexer :: String -> [Token]
lexer = tokenizarIMP
