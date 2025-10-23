{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Automata where

import Thompson (NFAe(..), epsilonClosure, epsilonClosureSet)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Bifunctor (first, second)
import Control.Arrow ((>>>))

-- core algebraic structures

-- estado es un functor sobre el alfabeto de transicion
newtype State q = State { getState :: q }
  deriving (Eq, Ord, Show, Functor)

-- transicion como profunctor: Q x Sigma -> P(Q) para nfa, Q x Sigma -> Q para dfa
-- mantenemos la estructura general pero parametrizada en el codominio

data Transition q a r = Transition { runTransition :: Map (q, a) r }
  deriving (Eq, Show, Functor)

-- automata como coalgebra: Q -> (Bool, Sigma -> Q)

data Automaton q a r = Automaton
  { states      :: Set q
  , alphabet    :: Set a
  , transitions :: Transition q a r
  , start       :: q
  , accepting   :: Set q
  } deriving (Eq, Show)

-- instancias especificas, nfa sin epsilon: estados simples, transiciones no deterministas

type NFA = Automaton Int Char (Set Int)

-- dfa: estados como conjuntos de estados nfa, transiciones deterministas
type DFA = Automaton (Set Int) Char (Set Int)

-- dfa con nombres enteros: isomorfismo via biyeccion
data DFAInt = DFAInt
  { dfaCore    :: Automaton Int Char Int
  , stateIso   :: Map Int (Set Int)  -- isomorfismo Q_int <-> P(Q_nfa)
  } deriving (Eq, Show)

-- eliminacion de epsilon: NFAe -> NFA

-- la epsilon-cerradura induce un comonad sobre estados

removeEpsilon :: NFAe -> NFA

removeEpsilon nfae = Automaton
  { states      = nfaeStates nfae
  , alphabet    = nfaeAlphabet nfae
  , transitions = Transition newTrans
  , start       = nfaeStart nfae
  , accepting   = newAccept
  }
  where
    -- transicion derivada: delta'(q,a) = eps-closure(delta(eps-closure(q), a))
    newTrans = Map.fromList
      [ ((s, c), epsClosureSet nfae directReach)
      | s <- Set.toList (nfaeStates nfae)
      , c <- Set.toList (nfaeAlphabet nfae)
      , let eClosure = epsilonClosure nfae s
      , let directReach = Set.unions
              [ Map.findWithDefault Set.empty (q, Just c) (nfaeTransitions nfae)
              | q <- Set.toList eClosure
              ]
      , not (Set.null directReach)
      ]
    
    -- F' = { q | eps-closure(q) intersecta F }
    newAccept = Set.filter
      (epsilonClosure nfae >>> flip Set.intersection (nfaeAccept nfae) >>> not . Set.null)
      (nfaeStates nfae)

    epsClosureSet = epsilonClosureSet

-- construccion de subconjuntos: NFA -> DFA (functor potencia) -- construccion por punto fijo: menor automata que simula el nfa

subsetConstruction :: NFA -> DFA
subsetConstruction nfa = Automaton
  { states      = reachable
  , alphabet    = alphabet nfa
  , transitions = Transition dfaTrans
  , start       = q0
  , accepting   = Set.filter (intersects $ accepting nfa) reachable
  }
  where
    q0 = Set.singleton (start nfa)
    
    -- punto fijo: iteracion hasta clausura
    (reachable, dfaTrans) = fixpoint (Set.singleton q0) q0 Map.empty
    
    -- paso de clausura: explora un estado y descubre vecinos
    explore :: Set Int -> Map (Set Int, Char) (Set Int)
            -> [(Set Int, Map (Set Int, Char) (Set Int))]
    explore current trans =
      let edges = 
            [ ((current, c), target)
            | c <- Set.toList (alphabet nfa)
            , let target = move nfa current c
            , not (Set.null target)
            ]
          newStates = map snd edges
          newTrans = Map.union trans (Map.fromList edges)
      in [(s, newTrans) | s <- newStates]
    
    -- punto fijo: clausura transitiva
    fixpoint :: Set (Set Int) -> Set Int -> Map (Set Int, Char) (Set Int)
             -> (Set (Set Int), Map (Set Int, Char) (Set Int))
    fixpoint visited current trans
      | Set.null current = (visited, trans)
      | otherwise = 
          let explored = explore current trans
              newStates = Set.fromList $ map fst explored
              newTrans = foldr (flip Map.union . snd) trans explored
              unvisited = Set.difference newStates visited
              allVisited = Set.union visited unvisited
          in if Set.null unvisited
             then (allVisited, newTrans)
             else Set.foldl' (\(v, t) s -> fixpoint v s t) (allVisited, newTrans) unvisited

-- transicion del nfa: delta*(Q, a) = Union_{q in Q} delta(q, a)
move :: NFA -> Set Int -> Char -> Set Int
move nfa qs a = Set.unions
  [ Map.findWithDefault Set.empty (q, a) (runTransition $ transitions nfa)
  | q <- Set.toList qs
  ]

-- predicado de interseccion no vacia
intersects :: Ord a => Set a -> Set a -> Bool
intersects xs ys = not . Set.null $ Set.intersection xs ys

-- isomorfismo DFA -> DFAInt (para minimizacion)

-- biyeccion entre P(Q) y {0,1,...,n-1}
dfaToInt :: DFA -> DFAInt
dfaToInt dfa = DFAInt
  { dfaCore = Automaton
      { states      = Set.fromList [0 .. n-1]
      , alphabet    = alphabet dfa
      , transitions = Transition intTrans
      , start       = toInt (start dfa)
      , accepting   = Set.map toInt (accepting dfa)
      }
  , stateIso = iso
  }
  where
    stateList = Set.toList (states dfa)
    n = length stateList
    
    -- isomorfismo directo e inverso
    toInt s = fromInt Map.! s
    fromInt = Map.fromList (zip stateList [0..])
    iso = Map.fromList (zip [0..] stateList)
    
    -- transiciones via isomorfismo (DFA tiene transiciones deterministas)
    intTrans = Map.fromList
      [ ((toInt from, c), toInt to)
      | ((from, c), to) <- Map.toList . runTransition $ transitions dfa
      ]

-- printing (minimo, para depuracion)

prettyNFA :: NFA -> String
prettyNFA = prettyAutomaton "NFA" show show

prettyDFA :: DFA -> String  
prettyDFA = prettyAutomaton "DFA" (show . Set.toList) show

prettyDFAInt :: DFAInt -> String
prettyDFAInt (DFAInt core iso) = prettyAutomaton "DFAInt" show show core

prettyAutomaton :: (Show q, Show a, Show r, Ord q, Ord a) 
                => String -> (q -> String) -> (r -> String) -> Automaton q a r -> String
prettyAutomaton name showState showTarget auto = unlines $
  [ name ++ ":"
  , "  Q = " ++ show (Set.size $ states auto)
  , "  Sigma = " ++ show (Set.toList $ alphabet auto)
  , "  q0 = " ++ showState (start auto)
  , "  F = " ++ show (Set.size $ accepting auto)
  , "  delta:"
  ] ++
  [ "    " ++ showState q ++ " --" ++ show a ++ "--> " ++ showTarget r
  | ((q, a), r) <- Map.toList . runTransition $ transitions auto
  ]
