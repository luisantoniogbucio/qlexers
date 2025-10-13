module LexerSQL where

import TokenSQL
import RegExp
import NFA
import DFA
import Minimizar
import MDD
import Data.Char (isSpace, isAlpha, isAlphaNum, isDigit) 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


-- Crea un lexer a partir de patrones de tokens
crearLexerSQL :: [(RegExp, TokenSQL)] -> [(MDD TokenSQL, TokenSQL)]
crearLexerSQL patrones =
  [ (crearMDDParaPatron regex, token) | (regex, token) <- patrones ]
  where
    crearMDDParaPatron regex =
      let nfa = construirNFA regex
          dfa = nfaADFA nfa
          dfaInt = dfaAEnteros dfa
          dfaMin = minimizarDFA dfaInt
          mapeoTokens = Map.fromList
            [(estado, TIdentifier "temp") | estado <- Set.toList (estadosFinalesInt dfaMin)]
      in dfaAMDD dfaMin mapeoTokens

saltarEspacios :: String -> String
saltarEspacios = dropWhile (`elem` " \t\n\r") 


tokenizarSQL :: [(MDD TokenSQL, TokenSQL)] -> String -> [TokenSQL]
tokenizarSQL lexers entrada = tokenizarAux [] (saltarEspacios entrada)
  where
    tokenizarAux tokens [] = reverse tokens
    tokenizarAux tokens input =
      case encontrarMejorCoincidenciaSQL lexers input of
        Just (token, restante) -> tokenizarAux (token:tokens) (saltarEspacios restante)
        Nothing -> error $ "Carácter no reconocido: " ++ take 1 input


encontrarMejorCoincidenciaSQL :: [(MDD TokenSQL, TokenSQL)] -> String -> Maybe (TokenSQL, String)
encontrarMejorCoincidenciaSQL lexers input =
  let coincidencias = [ (token, resto, texto, length texto)
                      | (mdd, token) <- lexers
                      , Just (_, resto, texto) <- [ejecutarMDD mdd input]
                      ]
      -- Tomar la coincidencia más larga (maximal munch)
      mejores = if null coincidencias
               then []
               else let maxLen = maximum (map (\(_, _, _, len) -> len) coincidencias)
                        in filter (\(_, _, _, len) -> len == maxLen) coincidencias
      -- Resolución de ambigüedad por prioridad (usa la primera coincidencia más larga)
  in case mejores of
       ((token, resto, texto, _):_) -> Just (procesarTokenSQL token texto, resto)
       [] -> Nothing


-- Procesa el token con el texto capturado para TIdentifier y TNumber
procesarTokenSQL :: TokenSQL -> String -> TokenSQL
procesarTokenSQL (TIdentifier _) texto = TIdentifier texto
procesarTokenSQL (TNumber _) texto = TNumber (read texto)
procesarTokenSQL token _ = token

patronesSQL :: [(RegExp, TokenSQL)]
patronesSQL =
  [)
    (cadena "<=", TLeqSQL)    -- <=
  , (cadena ">=", TGeq)       -- >=
  , (cadena "!=", TNeq)       -- !=
  , (cadena "<>", TNeq)       -- <>
  , (cadena "SELECT", TSELECT)
  , (cadena "FROM", TFROM)
  , (cadena "WHERE", TWHERE)
  , (cadena "INSERT", TINSERT)
  , (cadena "INTO", TINTO)
  , (cadena "VALUES", TVALUES)
  , (cadena "UPDATE", TUPDATE)
  , (cadena "SET", TSET)
  , (cadena "DELETE", TDELETE)
  , (cadena "CREATE", TCREATE)
  , (cadena "TABLE", TTABLE)
  , (cadena "DROP", TDROP)
  , (cadena "ALTER", TALTER)
  , (cadena "AND", TAND)
  , (cadena "OR", TOR)
  , (cadena "NOT", TNOT)
  , (cadena "NULL", TNULL)
  , (cadena "ORDER", TORDER)
  , (cadena "BY", TBY)
  , (cadena "GROUP", TGROUP)
  , (cadena "HAVING", THAVING)
  , (cadena "JOIN", TJOIN)
  , (cadena "INNER", TINNER)
  , (cadena "LEFT", TLEFT)
  , (cadena "RIGHT", TRIGHT)
  , (cadena "ON", TON)
  , (cadena "LIKE", TLike)

  , (Concat (Char '\'') (Concat (Star (Union (Char ' ') (Union letra digito))) (Char '\'')), TString "")
  , (unoOMas digito, TNumber 0) -- Números
  , (Concat letra (Star (Union letra digito)), TIdentifier "") 
  , (Char '=', TEq)
  , (Char '<', TLt)
  , (Char '>', TGt)
  , (Char ',', TComma)
  , (Char ';', TSemicolon)
  , (Char '(', TLParen)
  , (Char ')', TRParen)
  , (Char '*', TAsterisk)
  ]

-- Construir el lexer completo para SQL
construirLexerSQL :: [(MDD TokenSQL, TokenSQL)]
construirLexerSQL = crearLexerSQL patronesSQL

-- Función principal
lexerSQL :: String -> [TokenSQL]
lexerSQL = tokenizarSQL construirLexerSQL
