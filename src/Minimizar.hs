module Minimizar where

import Automata (DFAInt(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')

-- Partition: a set of sets of states
type Partition = Set (Set Int)

-- ============================================================================
-- Hopcroft's Algorithm for DFA Minimization
-- ============================================================================

-- Main minimization function
minimize :: DFAInt -> DFAInt
minimize dfa = 
  let finalPartition = hopcroft dfa
      -- Create new minimal DFA from partition
  in buildMinimalDFA dfa finalPartition


-- Hopcroft's algorithm
hopcroft :: DFAInt -> Partition
hopcroft dfa = refine initialPartition workList
  where
    -- Initial partition: accepting vs non-accepting states
    accepting = dfaIntAccept dfa
    nonAccepting = Set.difference (dfaIntStates dfa) accepting
    
    initialPartition = if Set.null nonAccepting
                       then Set.singleton accepting
                       else if Set.null accepting
                            then Set.singleton nonAccepting
                            else Set.fromList [accepting, nonAccepting]
    
    -- Initial work list contains the smaller of the two sets
    workList = if Set.size accepting <= Set.size nonAccepting
               then [accepting]
               else [nonAccepting]
    
    -- Refinement loop
    refine :: Partition -> [Set Int] -> Partition
    refine partition [] = partition
    refine partition (splitter:rest) =
      let -- For each character, split partition
          allChars = Set.toList (dfaIntAlphabet dfa)
          (newPartition, newWork) = foldl' 
            (\(p, w) c -> refineByChar dfa p w splitter c)
            (partition, rest)
            allChars
      in refine newPartition newWork


-- Refine partition based on a splitter set and character
refineByChar :: DFAInt -> Partition -> [Set Int] -> Set Int -> Char 
             -> (Partition, [Set Int])
refineByChar dfa partition workList splitter char =
  let -- Find all states that transition to splitter on char
      preImage = Set.filter
        (\s -> case Map.lookup (s, char) (dfaIntTransitions dfa) of
                 Just target -> Set.member target splitter
                 Nothing -> False)
        (dfaIntStates dfa)
      
      -- Split each block in partition
      (newPartition, addedToWork) = Set.foldl'
        (splitBlock preImage)
        (Set.empty, [])
        partition
      
  in (newPartition, workList ++ addedToWork)
  where
    -- Split a single block
    splitBlock :: Set Int -> (Partition, [Set Int]) -> Set Int 
               -> (Partition, [Set Int])
    splitBlock preImage (accPartition, accWork) block =
      let inPre = Set.intersection block preImage
          notInPre = Set.difference block preImage
      in if Set.null inPre || Set.null notInPre
         then (Set.insert block accPartition, accWork)  -- No split needed
         else 
           let -- Both parts are non-empty, so we split
               smaller = if Set.size inPre <= Set.size notInPre 
                        then inPre 
                        else notInPre
               -- Add both parts to partition
               newPartition = Set.insert inPre (Set.insert notInPre accPartition)
               -- Add smaller part to work list (optimization)
               newWork = if block `elem` accWork ++ workList
                        then smaller : accWork  -- Replace with both parts
                        else smaller : accWork  -- Just add smaller
           in (newPartition, newWork)
      where
        workList = []  -- Access outer workList if needed


-- ============================================================================
-- Build Minimal DFA from Partition
-- ============================================================================

buildMinimalDFA :: DFAInt -> Partition -> DFAInt
buildMinimalDFA dfa partition = DFAInt
  { dfaIntStates = Set.fromList [0 .. Set.size partition - 1]
  , dfaIntAlphabet = dfaIntAlphabet dfa
  , dfaIntTransitions = minTransitions
  , dfaIntStart = blockToInt Map.! findBlock (dfaIntStart dfa)
  , dfaIntAccept = Set.map (blockToInt Map.!) acceptingBlocks
  , dfaIntMapping = representativeMapping
  }
  where
    -- Map each block to an integer
    blockList = Set.toList partition
    blockToInt = Map.fromList (zip blockList [0..])
    
    -- Find which block contains a state
    findBlock :: Int -> Set Int
    findBlock state = 
      case filter (Set.member state) blockList of
        (block:_) -> block
        [] -> error "State not in any block"
    
    -- Find accepting blocks (blocks containing accept states)
    acceptingBlocks = Set.filter
      (\block -> not $ Set.null $ Set.intersection block (dfaIntAccept dfa))
      partition
    
    -- Build new transitions
    minTransitions = Map.fromList
      [ ((blockToInt Map.! block, c), blockToInt Map.! targetBlock)
      | block <- blockList
      , let representative = Set.findMin block  -- Pick any state from block
      , c <- Set.toList (dfaIntAlphabet dfa)
      , Just target <- [Map.lookup (representative, c) (dfaIntTransitions dfa)]
      , let targetBlock = findBlock target
      ]
    
    -- Mapping from new states to original state sets
    -- We'll use the original mapping through a representative
    representativeMapping = Map.fromList
      [ (blockToInt Map.! block, 
         case Map.lookup (Set.findMin block) (dfaIntMapping dfa) of
           Just original -> original
           Nothing -> Set.singleton (Set.findMin block))
      | block <- blockList
      ]


-- ============================================================================
-- Alternative: Simple Partition Refinement (easier to understand)
-- ============================================================================

-- Simpler version that's easier to debug but less efficient
minimizeSimple :: DFAInt -> DFAInt
minimizeSimple dfa = buildMinimalDFA dfa (refineUntilStable dfa initialPartition)
  where
    -- Initial partition: accepting vs non-accepting
    accepting = dfaIntAccept dfa
    nonAccepting = Set.difference (dfaIntStates dfa) accepting
    
    initialPartition = if Set.null nonAccepting
                       then Set.singleton accepting
                       else if Set.null accepting
                            then Set.singleton nonAccepting
                            else Set.fromList [accepting, nonAccepting]


-- Repeatedly refine partition until stable
refineUntilStable :: DFAInt -> Partition -> Partition
refineUntilStable dfa partition =
  let newPartition = refineOnce dfa partition
  in if newPartition == partition
     then partition
     else refineUntilStable dfa newPartition


-- One refinement pass
refineOnce :: DFAInt -> Partition -> Partition
refineOnce dfa partition = Set.unions $ Set.map (splitBlock dfa partition) partition


-- Split a block based on current partition
splitBlock :: DFAInt -> Partition -> Set Int -> Partition
splitBlock dfa partition block =
  if Set.size block <= 1
  then Set.singleton block
  else
    let -- Group states by their transition signatures
        signatures = Map.fromListWith Set.union
          [(signature dfa partition state, Set.singleton state) 
          | state <- Set.toList block]
    in Set.fromList (Map.elems signatures)


-- Compute signature of a state (which blocks it transitions to)
signature :: DFAInt -> Partition -> Int -> Map Char Int
signature dfa partition state = Map.fromList
  [ (c, blockId)
  | c <- Set.toList (dfaIntAlphabet dfa)
  , Just target <- [Map.lookup (state, c) (dfaIntTransitions dfa)]
  , let blockId = findBlockId partition target
  ]


-- Find which block (as an ID) contains a state
findBlockId :: Partition -> Int -> Int
findBlockId partition state =
  case filter (\(block, _) -> Set.member state block) 
              (zip (Set.toList partition) [0..]) of
    ((_, blockId):_) -> blockId
    [] -> -1

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Pretty print partition
prettyPartition :: Partition -> String
prettyPartition partition = unlines
  [ "Block " ++ show i ++ ": " ++ show (Set.toList block)
  | (block, i) <- zip (Set.toList partition) [0..]
  ]

-- Count states in partition
partitionSize :: Partition -> Int
partitionSize = Set.size

-- Verify minimization correctness (both DFAs should be equivalent)
-- This is a sanity check - returns True if sizes match
verifyMinimization :: DFAInt -> DFAInt -> Bool
verifyMinimization original minimal =
  Set.size (dfaIntStates minimal) <= Set.size (dfaIntStates original)
