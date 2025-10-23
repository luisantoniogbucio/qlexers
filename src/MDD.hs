{-# LANGUAGE OverloadedStrings #-}
module MDD where

import Token (TokenType(..))
import Automata (DFAInt(..), Automaton(..), Transition(..))
import Minimizar (minimize)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.List (foldl1')

-- mdd: maquina discriminadora multi-token
-- combina multiples dfas minimizados, uno por tipo de token
data MDD = MDD
  { mddAutomata :: [(TokenType, DFAInt, Int)]  -- (tipo, dfa, prioridad)
  , mddAlphabet :: Set Char                     -- alfabeto combinado
  } deriving (Show, Eq)

-- resultado de intento de matching
data MatchResult = MatchResult
  { matchToken  :: TokenType
  , matchLength :: Int
  , matchText   :: String
  } deriving (Show, Eq)

-- construccion de mdd desde especificaciones
-- prioridad mayor = mayor precedencia (ej: keywords > identifiers)
buildMDD :: [(TokenType, DFAInt, Int)] -> MDD
buildMDD automata = MDD
  { mddAutomata = automata
  , mddAlphabet = Set.unions [alphabet . dfaCore $ dfa | (_, dfa, _) <- automata]
  }

-- agregar token a mdd existente
addToken :: MDD -> TokenType -> DFAInt -> Int -> MDD
addToken mdd tokenType dfa priority = MDD
  { mddAutomata = (tokenType, dfa, priority) : mddAutomata mdd
  , mddAlphabet = Set.union (mddAlphabet mdd) (alphabet . dfaCore $ dfa)
  }

-- mdd vacio
emptyMDD :: MDD
emptyMDD = MDD [] Set.empty

-- maximal munch con backtracking
-- intenta matchear el token mas largo al inicio de la entrada
-- retorna: Just (token, longitud, resto) o Nothing
maximumMunch :: MDD -> String -> Maybe (TokenType, Int, String)
maximumMunch mdd input = 
  case findLongestMatch mdd input of
    Just (tokenType, len, _) -> Just (tokenType, len, drop len input)
    Nothing -> Nothing

-- encontrar el match mas largo entre todos los automatas
findLongestMatch :: MDD -> String -> Maybe (TokenType, Int, String)
findLongestMatch mdd input =
  let -- intentar match con cada automata
      matches = mapMaybe tryMatch (mddAutomata mdd)
      
      tryMatch (tt, dfa, prio) = 
        case matchDFA dfa input of
          Just len -> Just (tt, len, take len input, prio)
          Nothing -> Nothing
      
  in if null matches
     then Nothing
     else 
       -- seleccionar mejor match: (1) mas largo, (2) mayor prioridad
       let (tt, len, text, _) = foldl1' compareMatches matches
       in Just (tt, len, text)
  where
    compareMatches m1@(_, len1, _, prio1) m2@(_, len2, _, prio2)
      | len1 > len2 = m1
      | len1 < len2 = m2
      | prio1 > prio2 = m1
      | otherwise = m2

-- matching de dfa con backtracking
-- retorna longitud del match mas largo aceptado, o Nothing
matchDFA :: DFAInt -> String -> Maybe Int
matchDFA dfa input = go (start core) input 0 Nothing
  where
    core = dfaCore dfa
    Transition trans = transitions core
    acceptStates = accepting core
    
    -- go: estado_actual entrada consumida ultimo_accept
    go currentState remaining consumed lastAccept =
      let newLastAccept = if Set.member currentState acceptStates
                         then Just consumed
                         else lastAccept
      in case remaining of
           [] -> newLastAccept
           (c:cs) -> 
             case Map.lookup (currentState, c) trans of
               Just nextState -> go nextState cs (consumed + 1) newLastAccept
               Nothing -> newLastAccept

-- simulacion paso a paso (debugging)
stepDFA :: DFAInt -> Int -> Char -> Maybe Int
stepDFA dfa state char = 
  let Transition trans = transitions . dfaCore $ dfa
  in Map.lookup (state, char) trans

-- verificar si un estado es aceptador
isAcceptingState :: DFAInt -> Int -> Bool
isAcceptingState dfa state = Set.member state (accepting . dfaCore $ dfa)

-- obtener todos los estados siguientes posibles
nextStates :: DFAInt -> Int -> [(Char, Int)]
nextStates dfa state = 
  let core = dfaCore dfa
      Transition trans = transitions core
  in [ (c, nextState)
     | c <- Set.toList (alphabet core)
     , Just nextState <- [Map.lookup (state, c) trans]
     ]

-- reconocimiento de tokens en cadena completa
-- tokenizador simple usando mdd
recognizeTokens :: MDD -> String -> Either String [TokenType]
recognizeTokens mdd = go []
  where
    go acc [] = Right (reverse acc)
    go acc str = 
      case maximumMunch mdd str of
        Just (tokenType, len, remaining) -> go (tokenType : acc) remaining
        Nothing -> Left $ "lexical error at: " ++ take 20 str

-- niveles de prioridad estandar para imp
priorityKeyword, priorityOperator, priorityIdentifier :: Int
priorityNumber, priorityDelimiter, priorityWhitespace :: Int
priorityKeyword     = 100
priorityOperator    = 90
priorityIdentifier  = 80
priorityNumber      = 80
priorityDelimiter   = 70
priorityWhitespace  = 10

-- utilidades
mddSize :: MDD -> Int
mddSize = length . mddAutomata

mddTokenTypes :: MDD -> [TokenType]
mddTokenTypes mdd = [tt | (tt, _, _) <- mddAutomata mdd]

-- pretty printing
prettyMDD :: MDD -> String
prettyMDD mdd = unlines $
  [ "MDD with " ++ show (mddSize mdd) ++ " automata"
  , "Combined alphabet: " ++ show (Set.toList $ mddAlphabet mdd)
  , "Token types:"
  ] ++
  [ "  - " ++ show tt ++ " (priority: " ++ show prio ++ ", states: " ++ 
    show (Set.size . states . dfaCore $ dfa) ++ ")"
  | (tt, dfa, prio) <- mddAutomata mdd
  ]

-- debugging: cual dfa matchearia la entrada
debugMatch :: MDD -> String -> [(TokenType, Maybe Int, Int)]
debugMatch mdd input =
  [ (tt, matchDFA dfa input, prio)
  | (tt, dfa, prio) <- mddAutomata mdd
  ]
