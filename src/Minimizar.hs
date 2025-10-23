module Minimizar where

import Automata (DFAInt(..), Automaton(..), Transition(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

-- particion: conjunto de conjuntos de estados
type Partition = Set (Set Int)

-- minimizacion via hopcroft: DFAInt -> DFAInt minimal
minimize :: DFAInt -> DFAInt
minimize dfa = buildMinimalDFA dfa (hopcroft dfa)

-- algoritmo de hopcroft
hopcroft :: DFAInt -> Partition
hopcroft dfa = refine initialPartition workList
  where
    core = dfaCore dfa
    accepting' = accepting core
    nonAccepting = Set.difference (states core) accepting'
    
    -- particion inicial: estados aceptadores vs no aceptadores
    initialPartition = case (Set.null nonAccepting, Set.null accepting') of
      (True, _) -> Set.singleton accepting'
      (_, True) -> Set.singleton nonAccepting
      _ -> Set.fromList [accepting', nonAccepting]
    
    -- lista de trabajo inicial: el conjunto mas pequeno
    workList = if Set.size accepting' <= Set.size nonAccepting
               then [accepting']
               else [nonAccepting]
    
    -- bucle de refinamiento
    refine :: Partition -> [Set Int] -> Partition
    refine partition [] = partition
    refine partition (splitter:rest) =
      let allChars = Set.toList (alphabet core)
          (newPartition, newWork) = foldl' 
            (\(p, w) c -> refineByChar dfa p w splitter c)
            (partition, rest)
            allChars
      in refine newPartition newWork

-- refinamiento por caracter: particiona segun pre-imagen
refineByChar :: DFAInt -> Partition -> [Set Int] -> Set Int -> Char 
             -> (Partition, [Set Int])
refineByChar dfa partition workList splitter char =
  let core = dfaCore dfa
      Transition trans = transitions core
      
      -- pre-imagen: estados que transitan a splitter con char
      preImage = Set.filter
        (\s -> case Map.lookup (s, char) trans of
                 Just target -> Set.member target splitter
                 Nothing -> False)
        (states core)
      
      -- dividir cada bloque en la particion
      (newPartition, addedToWork) = Set.foldl'
        (splitBlock preImage workList)
        (Set.empty, [])
        partition
      
  in (newPartition, workList ++ addedToWork)
  where
    -- dividir un bloque individual
    splitBlock :: Set Int -> [Set Int] -> (Partition, [Set Int]) -> Set Int 
               -> (Partition, [Set Int])
    splitBlock preImage wl (accPartition, accWork) block =
      let inPre = Set.intersection block preImage
          notInPre = Set.difference block preImage
      in if Set.null inPre || Set.null notInPre
         then (Set.insert block accPartition, accWork)
         else 
           let smaller = if Set.size inPre <= Set.size notInPre then inPre else notInPre
               newPartition = Set.insert inPre (Set.insert notInPre accPartition)
               newWork = if block `elem` wl
                        then smaller : accWork
                        else smaller : accWork
           in (newPartition, newWork)

-- construccion del dfa minimo desde la particion
buildMinimalDFA :: DFAInt -> Partition -> DFAInt
buildMinimalDFA dfa partition = DFAInt
  { dfaCore = Automaton
      { states = Set.fromList [0 .. Set.size partition - 1]
      , alphabet = alphabet core
      , transitions = Transition minTransitions
      , start = blockToInt Map.! findBlock (start core)
      , accepting = Set.map (blockToInt Map.!) acceptingBlocks
      }
  , stateIso = representativeMapping
  }
  where
    core = dfaCore dfa
    Transition trans = transitions core
    
    -- mapeo de bloques a enteros
    blockList = Set.toList partition
    blockToInt = Map.fromList (zip blockList [0..])
    
    -- buscar el bloque que contiene un estado
    findBlock :: Int -> Set Int
    findBlock state = 
      case filter (Set.member state) blockList of
        (block:_) -> block
        [] -> error $ "state " ++ show state ++ " not in any block"
    
    -- bloques aceptadores
    acceptingBlocks = Set.filter
      (\block -> not . Set.null $ Set.intersection block (accepting core))
      partition
    
    -- transiciones del dfa minimo
    minTransitions = Map.fromList
      [ ((blockToInt Map.! block, c), blockToInt Map.! targetBlock)
      | block <- blockList
      , let representative = Set.findMin block
      , c <- Set.toList (alphabet core)
      , Just target <- [Map.lookup (representative, c) trans]
      , let targetBlock = findBlock target
      ]
    
    -- mapeo de estados nuevos a conjuntos originales
    representativeMapping = Map.fromList
      [ (blockToInt Map.! block, 
         case Map.lookup (Set.findMin block) (stateIso dfa) of
           Just original -> original
           Nothing -> Set.singleton (Set.findMin block))
      | block <- blockList
      ]

-- version simple: refinamiento hasta punto fijo
minimizeSimple :: DFAInt -> DFAInt
minimizeSimple dfa = buildMinimalDFA dfa (refineUntilStable dfa initialPartition)
  where
    core = dfaCore dfa
    accepting' = accepting core
    nonAccepting = Set.difference (states core) accepting'
    
    initialPartition = case (Set.null nonAccepting, Set.null accepting') of
      (True, _) -> Set.singleton accepting'
      (_, True) -> Set.singleton nonAccepting
      _ -> Set.fromList [accepting', nonAccepting]

-- punto fijo: refinar hasta estabilidad
refineUntilStable :: DFAInt -> Partition -> Partition
refineUntilStable dfa partition =
  let newPartition = refineOnce dfa partition
  in if newPartition == partition
     then partition
     else refineUntilStable dfa newPartition

-- una pasada de refinamiento
refineOnce :: DFAInt -> Partition -> Partition
refineOnce dfa partition = Set.unions $ Set.map (splitBlock dfa partition) partition

-- dividir un bloque segun firmas de transicion
splitBlock :: DFAInt -> Partition -> Set Int -> Partition
splitBlock dfa partition block
  | Set.size block <= 1 = Set.singleton block
  | otherwise =
      let signatures = Map.fromListWith Set.union
            [(signature dfa partition state, Set.singleton state) 
            | state <- Set.toList block]
      in Set.fromList (Map.elems signatures)

-- firma de un estado: a que bloques transita
signature :: DFAInt -> Partition -> Int -> Map Char Int
signature dfa partition state = 
  let core = dfaCore dfa
      Transition trans = transitions core
  in Map.fromList
    [ (c, blockId)
    | c <- Set.toList (alphabet core)
    , Just target <- [Map.lookup (state, c) trans]
    , let blockId = findBlockId partition target
    ]

-- encontrar el id del bloque que contiene un estado
findBlockId :: Partition -> Int -> Int
findBlockId partition state =
  case filter (\(block, _) -> Set.member state block) 
              (zip (Set.toList partition) [0..]) of
    ((_, blockId):_) -> blockId
    [] -> -1

-- pretty printing y utilidades
prettyPartition :: Partition -> String
prettyPartition partition = unlines
  [ "Block " ++ show i ++ ": " ++ show (Set.toList block)
  | (block, i) <- zip (Set.toList partition) [0..]
  ]

partitionSize :: Partition -> Int
partitionSize = Set.size

-- verificacion: el minimo debe tener menos o igual estados
verifyMinimization :: DFAInt -> DFAInt -> Bool
verifyMinimization original minimal =
  Set.size (states $ dfaCore minimal) <= Set.size (states $ dfaCore original)
