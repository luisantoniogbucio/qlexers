module Automata where

import Thompson (NFAe(..), epsilonClosure, epsilonClosureSet)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')

-- NFA (without epsilon transitions)
data NFA = NFA
  { nfaStates      :: Set Int
  , nfaAlphabet    :: Set Char
  , nfaTransitions :: Map (Int, Char) (Set Int)  -- (state, char) -> states
  , nfaStart       :: Int
  , nfaAccept      :: Set Int
  } deriving (Show, Eq)

-- DFA (deterministic finite automaton)
-- States are represented as sets of NFA states
data DFA = DFA
  { dfaStates      :: Set (Set Int)           -- each state is a set of NFA states
  , dfaAlphabet    :: Set Char
  , dfaTransitions :: Map (Set Int, Char) (Set Int)  -- (state, char) -> state
  , dfaStart       :: Set Int
  , dfaAccept      :: Set (Set Int)           -- accepting states
  } deriving (Show, Eq)

-- DFA with integer state names (for easier minimization)
data DFAInt = DFAInt
  { dfaIntStates      :: Set Int
  , dfaIntAlphabet    :: Set Char
  , dfaIntTransitions :: Map (Int, Char) Int  -- (state, char) -> state
  , dfaIntStart       :: Int
  , dfaIntAccept      :: Set Int
  , dfaIntMapping     :: Map Int (Set Int)    -- maps integer states back to NFA state sets
  } deriving (Show, Eq)


-- ============================================================================
-- STEP 1: Remove Epsilon Transitions (NFAε -> NFA)
-- ============================================================================

removeEpsilon :: NFAe -> NFA
removeEpsilon nfae = NFA
  { nfaStates = nfaeStates nfae
  , nfaAlphabet = nfaeAlphabet nfae
  , nfaTransitions = newTransitions
  , nfaStart = nfaeStart nfae
  , nfaAccept = newAccept
  }
  where
    -- For each state and character, find all reachable states
    -- by following epsilon closures
    newTransitions = Map.fromList
      [ ((s, c), reachable)
      | s <- Set.toList (nfaeStates nfae)
      , c <- Set.toList (nfaeAlphabet nfae)
      , let eClosure = epsilonClosure nfae s
      , let directReach = Set.unions
              [ Map.findWithDefault Set.empty (q, Just c) (nfaeTransitions nfae)
              | q <- Set.toList eClosure
              ]
      , let reachable = epsilonClosureSet nfae directReach
      , not (Set.null reachable)
      ]
    
    -- A state is accepting if its epsilon closure contains an accept state
    newAccept = Set.filter
      (\s -> not $ Set.null $ Set.intersection
        (epsilonClosure nfae s)
        (nfaeAccept nfae))
      (nfaeStates nfae)


-- ============================================================================
-- STEP 2: Subset Construction (NFA -> DFA)
-- ============================================================================

subsetConstruction :: NFA -> DFA
subsetConstruction nfa = DFA
  { dfaStates = dfaStatesSet
  , dfaAlphabet = nfaAlphabet nfa
  , dfaTransitions = dfaTransMap
  , dfaStart = startState
  , dfaAccept = acceptStates
  }
  where
    startState = Set.singleton (nfaStart nfa)
    
    -- Build DFA states and transitions using worklist algorithm
    (dfaStatesSet, dfaTransMap) = buildDFA (Set.singleton startState) startState Map.empty
    
    -- A DFA state is accepting if it contains any NFA accept state
    acceptStates = Set.filter
      (\s -> not $ Set.null $ Set.intersection s (nfaAccept nfa))
      dfaStatesSet
    
    -- Worklist algorithm to explore all reachable DFA states
    buildDFA :: Set (Set Int) -> Set Int -> Map (Set Int, Char) (Set Int) 
             -> (Set (Set Int), Map (Set Int, Char) (Set Int))
    buildDFA visited workset transitions
      | Set.null workset = (visited, transitions)
      | otherwise = 
          let -- Process all characters for current state
              current = workset
              newTransAndStates = 
                [ ((current, c), target)
                | c <- Set.toList (nfaAlphabet nfa)
                , let target = Set.unions
                        [ Map.findWithDefault Set.empty (s, c) (nfaTransitions nfa)
                        | s <- Set.toList current
                        ]
                , not (Set.null target)
                ]
              
              newTrans = Map.fromList newTransAndStates
              newStates = Set.fromList $ map snd newTransAndStates
              unvisited = Set.difference newStates visited
              
              allTrans = Map.union transitions newTrans
              allVisited = Set.union visited unvisited
              
          in if Set.null unvisited
             then (allVisited, allTrans)
             else foldl' (\(v, t) s -> buildDFA v s t) (allVisited, allTrans) (Set.toList unvisited)


-- ============================================================================
-- STEP 3: Convert DFA to DFAInt (for easier minimization)
-- ============================================================================

dfaToInt :: DFA -> DFAInt
dfaToInt dfa = DFAInt
  { dfaIntStates = Set.fromList [0 .. length stateList - 1]
  , dfaIntAlphabet = dfaAlphabet dfa
  , dfaIntTransitions = intTransitions
  , dfaIntStart = stateToInt Map.! dfaStart dfa
  , dfaIntAccept = Set.map (stateToInt Map.!) (dfaAccept dfa)
  , dfaIntMapping = Map.fromList (zip [0..] stateList)
  }
  where
    stateList = Set.toList (dfaStates dfa)
    stateToInt = Map.fromList (zip stateList [0..])
    
    intTransitions = Map.fromList
      [ ((stateToInt Map.! from, c), stateToInt Map.! to)
      | ((from, c), to) <- Map.toList (dfaTransitions dfa)
      ]


-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Pretty print NFA
prettyNFA :: NFA -> String
prettyNFA nfa = unlines $
  [ "States: " ++ show (Set.toList $ nfaStates nfa)
  , "Alphabet: " ++ show (Set.toList $ nfaAlphabet nfa)
  , "Start: " ++ show (nfaStart nfa)
  , "Accept: " ++ show (Set.toList $ nfaAccept nfa)
  , "Transitions:"
  ] ++
  [ show from ++ " --" ++ [c] ++ "--> " ++ show (Set.toList to)
  | ((from, c), to) <- Map.toList (nfaTransitions nfa)
  ]

-- Pretty print DFA (simplified - shows state sets as lists)
prettyDFA :: DFA -> String
prettyDFA dfa = unlines $
  [ "Number of states: " ++ show (Set.size $ dfaStates dfa)
  , "Alphabet: " ++ show (Set.toList $ dfaAlphabet dfa)
  , "Start: " ++ show (Set.toList $ dfaStart dfa)
  , "Accept states: " ++ show (length $ Set.toList $ dfaAccept dfa)
  , "Transitions: " ++ show (Map.size $ dfaTransitions dfa)
  ]

-- Pretty print DFAInt
prettyDFAInt :: DFAInt -> String
prettyDFAInt dfa = unlines $
  [ "States: " ++ show (Set.toList $ dfaIntStates dfa)
  , "Alphabet: " ++ show (Set.toList $ dfaIntAlphabet dfa)
  , "Start: " ++ show (dfaIntStart dfa)
  , "Accept: " ++ show (Set.toList $ dfaIntAccept dfa)
  , "Transitions:"
  ] ++
  [ show from ++ " --" ++ [c] ++ "--> " ++ show to
  | ((from, c), to) <- Map.toList (dfaIntTransitions dfa)
  ]
