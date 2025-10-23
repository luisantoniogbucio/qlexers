{-# LANGUAGE OverloadedStrings #-}
module Thompson where

import RegEx
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Bifunctor (first)

-- nfa con transiciones epsilon
data NFAe = NFAe
  { nfaeStates      :: Set Int
  , nfaeAlphabet    :: Set Char
  , nfaeTransitions :: Map (Int, Maybe Char) (Set Int)  -- Nothing = epsilon
  , nfaeStart       :: Int
  , nfaeAccept      :: Set Int
  } deriving (Show, Eq)

-- contador de estados como monad state
type Fresh = State Int

-- generador de estados frescos
fresh :: Fresh Int
fresh = state (\n -> (n, n + 1))

freshPair :: Fresh (Int, Int)
freshPair = (,) <$> fresh <*> fresh

-- construccion de thompson: RE -> NFAe
-- cada construccion preserva la propiedad: un estado inicial, uno final
thompson :: RegEx -> NFAe
thompson r = evalState (build r) 0

-- builder monadico: construye nfa usando estados frescos
build :: RegEx -> Fresh NFAe
build Empty = do
  (start, accept) <- freshPair
  return $ NFAe
    { nfaeStates = Set.fromList [start, accept]
    , nfaeAlphabet = Set.empty
    , nfaeTransitions = Map.singleton (start, Nothing) (Set.singleton accept)
    , nfaeStart = start
    , nfaeAccept = Set.singleton accept
    }

build Epsilon = build Empty

build (Char c) = do
  (start, accept) <- freshPair
  return $ NFAe
    { nfaeStates = Set.fromList [start, accept]
    , nfaeAlphabet = Set.singleton c
    , nfaeTransitions = Map.singleton (start, Just c) (Set.singleton accept)
    , nfaeStart = start
    , nfaeAccept = Set.singleton accept
    }

build (CharClass chars) = do
  (start, accept) <- freshPair
  let trans = Map.fromList [((start, Just c), Set.singleton accept) | c <- Set.toList chars]
  return $ NFAe
    { nfaeStates = Set.fromList [start, accept]
    , nfaeAlphabet = chars
    , nfaeTransitions = trans
    , nfaeStart = start
    , nfaeAccept = Set.singleton accept
    }

build (Range low high) = build (CharClass $ Set.fromList [low..high])

-- concatenacion: nfa1 · nfa2
-- conecta estados finales de nfa1 al inicial de nfa2 via epsilon
build (Concat r1 r2) = do
  nfa1 <- build r1
  nfa2 <- build r2
  
  let epsilonLinks = Map.fromList
        [((q, Nothing), Set.singleton (nfaeStart nfa2)) | q <- Set.toList (nfaeAccept nfa1)]
      
      trans = Map.unionsWith Set.union
        [nfaeTransitions nfa1, nfaeTransitions nfa2, epsilonLinks]
  
  return $ NFAe
    { nfaeStates = Set.union (nfaeStates nfa1) (nfaeStates nfa2)
    , nfaeAlphabet = Set.union (nfaeAlphabet nfa1) (nfaeAlphabet nfa2)
    , nfaeTransitions = trans
    , nfaeStart = nfaeStart nfa1
    , nfaeAccept = nfaeAccept nfa2
    }

-- union: nfa1 | nfa2
-- nuevo inicio con epsilon a ambos, ambos finales con epsilon a nuevo final
build (Union r1 r2) = do
  (newStart, newAccept) <- freshPair
  nfa1 <- build r1
  nfa2 <- build r2
  
  let startLinks = Map.singleton (newStart, Nothing)
                     (Set.fromList [nfaeStart nfa1, nfaeStart nfa2])
      
      acceptLinks1 = Map.fromList
        [((q, Nothing), Set.singleton newAccept) | q <- Set.toList (nfaeAccept nfa1)]
      
      acceptLinks2 = Map.fromList
        [((q, Nothing), Set.singleton newAccept) | q <- Set.toList (nfaeAccept nfa2)]
      
      trans = Map.unionsWith Set.union
        [nfaeTransitions nfa1, nfaeTransitions nfa2, startLinks, acceptLinks1, acceptLinks2]
  
  return $ NFAe
    { nfaeStates = Set.unions [nfaeStates nfa1, nfaeStates nfa2, Set.fromList [newStart, newAccept]]
    , nfaeAlphabet = Set.union (nfaeAlphabet nfa1) (nfaeAlphabet nfa2)
    , nfaeTransitions = trans
    , nfaeStart = newStart
    , nfaeAccept = Set.singleton newAccept
    }

-- cerradura de kleene: r*
-- nuevo inicio con epsilon al antiguo y al nuevo final (cero repeticiones)
-- finales antiguos con epsilon al inicio antiguo (loop) y al nuevo final
build (Star r) = do
  (newStart, newAccept) <- freshPair
  nfa <- build r
  
  let startLinks = Map.singleton (newStart, Nothing)
                     (Set.fromList [nfaeStart nfa, newAccept])
      
      loopLinks = Map.fromList
        [((q, Nothing), Set.fromList [nfaeStart nfa, newAccept]) 
         | q <- Set.toList (nfaeAccept nfa)]
      
      trans = Map.unionsWith Set.union
        [nfaeTransitions nfa, startLinks, loopLinks]
  
  return $ NFAe
    { nfaeStates = Set.union (nfaeStates nfa) (Set.fromList [newStart, newAccept])
    , nfaeAlphabet = nfaeAlphabet nfa
    , nfaeTransitions = trans
    , nfaeStart = newStart
    , nfaeAccept = Set.singleton newAccept
    }

-- cerradura positiva: r+ ≡ r · r*
build (Plus r) = build (Concat r (Star r))

-- epsilon-cerradura: menor punto fijo que contiene al estado
-- y es cerrado bajo transiciones epsilon
epsilonClosure :: NFAe -> Int -> Set Int
epsilonClosure nfa = fix . Set.singleton
  where
    fix visited = 
      let frontier = Set.unions
            [Map.findWithDefault Set.empty (q, Nothing) (nfaeTransitions nfa)
            | q <- Set.toList visited]
          visited' = Set.union visited frontier
      in if visited' == visited then visited else fix visited'

-- epsilon-cerradura de un conjunto: union de cerraduras individuales
epsilonClosureSet :: NFAe -> Set Int -> Set Int
epsilonClosureSet nfa = Set.unions . map (epsilonClosure nfa) . Set.toList

-- pretty printing (minimo)
prettyNFAe :: NFAe -> String
prettyNFAe nfa = unlines $
  [ "NFAe:"
  , "  Q = " ++ show (Set.size $ nfaeStates nfa)
  , "  Sigma = " ++ show (Set.toList $ nfaeAlphabet nfa)
  , "  q0 = " ++ show (nfaeStart nfa)
  , "  F = " ++ show (Set.toList $ nfaeAccept nfa)
  , "  delta:"
  ] ++
  [ "    " ++ show from ++ " --" ++ showLabel c ++ "--> " ++ show (Set.toList to)
  | ((from, c), to) <- Map.toList (nfaeTransitions nfa)
  ]
  where
    showLabel Nothing = "ε"
    showLabel (Just ch) = [ch]
