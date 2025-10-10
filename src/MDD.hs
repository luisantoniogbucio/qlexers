{-# LANGUAGE OverloadedStrings #-}
module MDD where

import Token (TokenType(..))
import Automata (DFAInt(..))
import Minimizar (minimize)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)

-- ============================================================================
-- MDD: Multi-token Discriminating Machine
-- ============================================================================

-- MDD combines multiple minimized DFAs, one for each token type
-- Each DFA recognizes one type of token with a priority
data MDD = MDD
  { mddAutomata  :: [(TokenType, DFAInt, Int)]  -- (token type, DFA, priority)
  , mddAlphabet  :: Set Char                     -- combined alphabet
  } deriving (Show, Eq)

-- Result of trying to match a token
data MatchResult = MatchResult
  { matchToken  :: TokenType
  , matchLength :: Int
  , matchText   :: String
  } deriving (Show, Eq)


-- ============================================================================
-- Build MDD from Token Specifications
-- ============================================================================

-- Build MDD from list of (TokenType, DFAInt, Priority)
-- Higher priority number = higher priority (e.g., keywords > identifiers)
buildMDD :: [(TokenType, DFAInt, Int)] -> MDD
buildMDD automata = MDD
  { mddAutomata = automata
  , mddAlphabet = Set.unions [dfaIntAlphabet dfa | (_, dfa, _) <- automata]
  }


-- Add a token specification to MDD
addToken :: MDD -> TokenType -> DFAInt -> Int -> MDD
addToken mdd tokenType dfa priority = MDD
  { mddAutomata = (tokenType, dfa, priority) : mddAutomata mdd
  , mddAlphabet = Set.union (mddAlphabet mdd) (dfaIntAlphabet dfa)
  }


-- Create empty MDD
emptyMDD :: MDD
emptyMDD = MDD [] Set.empty


-- ============================================================================
-- Maximal Munch with Backtracking
-- ============================================================================

-- Try to match the longest token starting at the beginning of input
-- Returns: Just (token, length, remaining_input) or Nothing
maximumMunch :: MDD -> String -> Maybe (TokenType, Int, String)
maximumMunch mdd input = 
  case findLongestMatch mdd input of
    Just (tokenType, len, text) -> Just (tokenType, len, drop len input)
    Nothing -> Nothing


-- Find the longest match among all automata
findLongestMatch :: MDD -> String -> Maybe (TokenType, Int, String)
findLongestMatch mdd input =
  let -- Try to match with each automaton
      matches = mapMaybe (\(tt, dfa, prio) -> 
                   case matchDFA dfa input of
                     Just len -> Just (tt, len, take len input, prio)
                     Nothing -> Nothing)
                (mddAutomata mdd)
      
  in if null matches
     then Nothing
     else 
       -- Select best match:
       -- 1. Longest match
       -- 2. If tie, highest priority
       let best = foldl1 comparMatches matches
       in Just (getTokenType best, getLength best, getText best)
  where
    comparMatches m1@(_, len1, _, prio1) m2@(_, len2, _, prio2)
      | len1 > len2 = m1
      | len1 < len2 = m2
      | prio1 > prio2 = m1  -- Higher priority wins
      | otherwise = m2
    
    getTokenType (tt, _, _, _) = tt
    getLength (_, len, _, _) = len
    getText (_, _, text, _) = text


-- ============================================================================
-- DFA Matching with Backtracking
-- ============================================================================

-- Match input against a single DFA
-- Returns length of longest accepting match, or Nothing
matchDFA :: DFAInt -> String -> Maybe Int
matchDFA dfa input = go (dfaIntStart dfa) input 0 Nothing
  where
    -- go: current_state input consumed_length last_accept_length
    go currentState remaining consumed lastAccept =
      let -- Check if current state is accepting
          newLastAccept = if Set.member currentState (dfaIntAccept dfa)
                         then Just consumed
                         else lastAccept
      in case remaining of
           [] -> newLastAccept  -- End of input
           (c:cs) -> 
             case Map.lookup (currentState, c) (dfaIntTransitions dfa) of
               Just nextState -> 
                 -- Continue matching
                 go nextState cs (consumed + 1) newLastAccept
               Nothing -> 
                 -- No transition - return last accept or Nothing
                 newLastAccept


-- ============================================================================
-- Simulate DFA step-by-step (useful for debugging)
-- ============================================================================

-- Single step of DFA simulation
stepDFA :: DFAInt -> Int -> Char -> Maybe Int
stepDFA dfa state char = Map.lookup (state, char) (dfaIntTransitions dfa)


-- Check if a state is accepting
isAcceptingState :: DFAInt -> Int -> Bool
isAcceptingState dfa state = Set.member state (dfaIntAccept dfa)


-- Get all possible next states from current state
nextStates :: DFAInt -> Int -> [(Char, Int)]
nextStates dfa state = 
  [ (c, nextState)
  | c <- Set.toList (dfaIntAlphabet dfa)
  , Just nextState <- [Map.lookup (state, c) (dfaIntTransitions dfa)]
  ]


-- ============================================================================
-- Token Recognition with Full Input
-- ============================================================================

-- Recognize all tokens in input string
-- This is a simple tokenizer using the MDD
recognizeTokens :: MDD -> String -> Either String [TokenType]
recognizeTokens mdd input = go input []
  where
    go [] acc = Right (reverse acc)
    go str acc = 
      case maximumMunch mdd str of
        Just (tokenType, len, remaining) ->
          go remaining (tokenType : acc)
        Nothing ->
          Left $ "Lexical error at: " ++ take 20 str


-- ============================================================================
-- Priority Management
-- ============================================================================

-- Standard priority levels for IMP tokens
priorityKeyword :: Int
priorityKeyword = 100

priorityOperator :: Int
priorityOperator = 90

priorityIdentifier :: Int
priorityIdentifier = 80

priorityNumber :: Int
priorityNumber = 80

priorityDelimiter :: Int
priorityDelimiter = 70

priorityWhitespace :: Int
priorityWhitespace = 10


-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Count total automata in MDD
mddSize :: MDD -> Int
mddSize = length . mddAutomata

-- Get all token types in MDD
mddTokenTypes :: MDD -> [TokenType]
mddTokenTypes mdd = [tt | (tt, _, _) <- mddAutomata mdd]

-- Pretty print MDD info
-- Pretty print MDD info
prettyMDD :: MDD -> String
prettyMDD mdd = unlines $
  [ "MDD with " ++ show (mddSize mdd) ++ " automata"
  , "Combined alphabet: " ++ show (Set.toList $ mddAlphabet mdd)
  , "Token types:"
  ] ++
  [ "  - " ++ show tt ++ " (priority: " ++ show prio ++ ", states: " ++ 
    show (Set.size $ dfaIntStates dfa) ++ ")"
  | (tt, dfa, prio) <- mddAutomata mdd
  ]

-- Find which DFA would match input (for debugging)
debugMatch :: MDD -> String -> [(TokenType, Maybe Int, Int)]
debugMatch mdd input =
  [ (tt, matchDFA dfa input, prio)
  | (tt, dfa, prio) <- mddAutomata mdd
  ]
  
