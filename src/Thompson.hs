{-# LANGUAGE OverloadedStrings #-}

module Thompson where

import RegEx
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- NFAε (NFA with epsilon transitions)
data NFAe = NFAe
  { nfaeStates      :: Set Int              -- set of states
  , nfaeAlphabet    :: Set Char             -- input alphabet
  , nfaeTransitions :: Map (Int, Maybe Char) (Set Int)  -- transitions: (state, char) -> states
                                            -- Nothing = epsilon transition
  , nfaeStart       :: Int                  -- start state
  , nfaeAccept      :: Set Int              -- accepting states
  } deriving (Show, Eq)

-- State counter for generating unique states
type StateCounter = Int

-- Thompson's Construction Algorithm
-- Converts a RegEx to an NFAε
thompson :: RegEx -> NFAe
thompson regex = nfa
  where
    (nfa, _) = thompsonWithCounter regex 0

-- Internal function that tracks state counter
thompsonWithCounter :: RegEx -> StateCounter -> (NFAe, StateCounter)

-- Case 1: Empty/Epsilon - single transition from start to accept
thompsonWithCounter Empty counter = 
  let start = counter
      accept = counter + 1
      states = Set.fromList [start, accept]
      trans = Map.singleton (start, Nothing) (Set.singleton accept)
  in (NFAe states Set.empty trans start (Set.singleton accept), counter + 2)

thompsonWithCounter Epsilon counter = 
  thompsonWithCounter Empty counter

-- Case 2: Single Character - transition on that character
thompsonWithCounter (Char c) counter =
  let start = counter
      accept = counter + 1
      states = Set.fromList [start, accept]
      alphabet = Set.singleton c
      trans = Map.singleton (start, Just c) (Set.singleton accept)
  in (NFAe states alphabet trans start (Set.singleton accept), counter + 2)

-- Case 3: Character Class [abc] - multiple transitions from same state
thompsonWithCounter (CharClass chars) counter =
  let start = counter
      accept = counter + 1
      states = Set.fromList [start, accept]
      alphabet = chars
      -- Create transition for each character in the class
      trans = Map.fromList [((start, Just c), Set.singleton accept) | c <- Set.toList chars]
  in (NFAe states alphabet trans start (Set.singleton accept), counter + 2)

-- Case 4: Range [a-z] - convert to CharClass and recurse
thompsonWithCounter (Range low high) counter =
  let chars = Set.fromList [low..high]
  in thompsonWithCounter (CharClass chars) counter

-- Case 5: Concatenation r1 · r2
thompsonWithCounter (Concat r1 r2) counter =
  let (nfa1, counter1) = thompsonWithCounter r1 counter
      (nfa2, counter2) = thompsonWithCounter r2 counter1
      
      -- Merge states and alphabets
      states = Set.union (nfaeStates nfa1) (nfaeStates nfa2)
      alphabet = Set.union (nfaeAlphabet nfa1) (nfaeAlphabet nfa2)
      
      -- Connect accept states of nfa1 to start of nfa2 with epsilon
      epsilonTrans = Map.fromList 
        [((acc, Nothing), Set.singleton (nfaeStart nfa2)) | acc <- Set.toList (nfaeAccept nfa1)]
      
      -- Merge all transitions
      trans = Map.unionWith Set.union (nfaeTransitions nfa1) 
              (Map.unionWith Set.union epsilonTrans (nfaeTransitions nfa2))
      
      start = nfaeStart nfa1
      accept = nfaeAccept nfa2
      
  in (NFAe states alphabet trans start accept, counter2)

-- Case 6: Union r1 | r2
thompsonWithCounter (Union r1 r2) counter =
  let (nfa1, counter1) = thompsonWithCounter r1 (counter + 2)  -- +2 for new start and accept
      (nfa2, counter2) = thompsonWithCounter r2 counter1
      
      newStart = counter
      newAccept = counter + 1
      
      -- Merge states
      states = Set.unions [nfaeStates nfa1, nfaeStates nfa2, 
                          Set.fromList [newStart, newAccept]]
      
      -- Merge alphabets
      alphabet = Set.union (nfaeAlphabet nfa1) (nfaeAlphabet nfa2)
      
      -- Epsilon from new start to both sub-NFAs
      startTrans = Map.singleton (newStart, Nothing) 
                   (Set.fromList [nfaeStart nfa1, nfaeStart nfa2])
      
      -- Epsilon from both accept states to new accept
      acceptTrans1 = Map.fromList 
        [((acc, Nothing), Set.singleton newAccept) | acc <- Set.toList (nfaeAccept nfa1)]
      acceptTrans2 = Map.fromList 
        [((acc, Nothing), Set.singleton newAccept) | acc <- Set.toList (nfaeAccept nfa2)]
      
      -- Merge all transitions
      trans = Map.unionsWith Set.union 
              [nfaeTransitions nfa1, nfaeTransitions nfa2, 
               startTrans, acceptTrans1, acceptTrans2]
      
  in (NFAe states alphabet trans newStart (Set.singleton newAccept), counter2)

-- Case 7: Kleene Star r*
thompsonWithCounter (Star r) counter =
  let (nfa, counter1) = thompsonWithCounter r (counter + 2)
      
      newStart = counter
      newAccept = counter + 1
      
      states = Set.union (nfaeStates nfa) (Set.fromList [newStart, newAccept])
      alphabet = nfaeAlphabet nfa
      
      -- Epsilon from new start to old start and new accept (for zero matches)
      startTrans = Map.singleton (newStart, Nothing) 
                   (Set.fromList [nfaeStart nfa, newAccept])
      
      -- Epsilon from accept states back to start (for repetition) and to new accept
      loopTrans = Map.fromList 
        [((acc, Nothing), Set.fromList [nfaeStart nfa, newAccept]) 
         | acc <- Set.toList (nfaeAccept nfa)]
      
      trans = Map.unionsWith Set.union 
              [nfaeTransitions nfa, startTrans, loopTrans]
      
  in (NFAe states alphabet trans newStart (Set.singleton newAccept), counter1)

-- Case 8: Plus r+ = r · r*
thompsonWithCounter (Plus r) counter =
  thompsonWithCounter (Concat r (Star r)) counter


-- Helper functions for working with NFAε

-- Get all states reachable via epsilon transitions from a state
epsilonClosure :: NFAe -> Int -> Set Int
epsilonClosure nfa state = go (Set.singleton state) (Set.singleton state)
  where
    go visited frontier
      | Set.null frontier = visited
      | otherwise =
          let newStates = Set.unions 
                [Map.findWithDefault Set.empty (s, Nothing) (nfaeTransitions nfa) 
                 | s <- Set.toList frontier]
              unvisited = Set.difference newStates visited
          in go (Set.union visited unvisited) unvisited

-- Get epsilon closure for a set of states
epsilonClosureSet :: NFAe -> Set Int -> Set Int
epsilonClosureSet nfa states = 
  Set.unions [epsilonClosure nfa s | s <- Set.toList states]

-- Pretty print NFAε (useful for debugging)
prettyNFAe :: NFAe -> String
prettyNFAe nfa = unlines $
  [ "States: " ++ show (Set.toList $ nfaeStates nfa)
  , "Alphabet: " ++ show (Set.toList $ nfaeAlphabet nfa)
  , "Start: " ++ show (nfaeStart nfa)
  , "Accept: " ++ show (Set.toList $ nfaeAccept nfa)
  , "Transitions:"
  ] ++ 
  [show from ++ " --" ++ showLabel c ++ "--> " ++ show (Set.toList to)
   | ((from, c), to) <- Map.toList (nfaeTransitions nfa)]
  where
    showLabel Nothing = "ε"
    showLabel (Just ch) = [ch]    
