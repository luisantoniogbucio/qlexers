module Minimizar where

import DFA
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (partition)

-- DFA con estados enteros (más fácil para minimizar)
data DFAInt = DFAInt
  { estadosInt :: Set Int                          -- Estados numerados
  , alfabetoInt :: Set Char                        -- Alfabeto
  , transicionesInt :: Map (Int, Char) Int         -- Transiciones deterministas
  , estadoInicialInt :: Int                        -- Estado inicial
  , estadosFinalesInt :: Set Int                   -- Estados finales
  , mapeoEstados :: Map Int (Set Int)              -- Mapeo a estados originales del NFA
  } deriving (Show, Eq)

-- Convierte DFA a DFAInt para facilitar minimización
dfaAEnteros :: DFA -> DFAInt
dfaAEnteros dfa = DFAInt
  { estadosInt = Set.fromList [0 .. length listaEstados - 1]
  , alfabetoInt = alfabetoDFA dfa
  , transicionesInt = transicionesEnteras
  , estadoInicialInt = estadoAEntero Map.! estadoInicialDFA dfa
  , estadosFinalesInt = Set.map (estadoAEntero Map.!) (estadosFinalesDFA dfa)
  , mapeoEstados = Map.fromList (zip [0..] listaEstados)
  }
  where
    listaEstados = Set.toList (estadosDFA dfa)
    estadoAEntero = Map.fromList (zip listaEstados [0..])
    
    transicionesEnteras = Map.fromList
      [ ((estadoAEntero Map.! desde, c), estadoAEntero Map.! hacia)
      | ((desde, c), hacia) <- Map.toList (transicionesDFA dfa)
      ]

-- Minimiza DFA usando algoritmo de particiones
minimizarDFA :: DFAInt -> DFAInt
minimizarDFA dfa = construirDFAMinimo dfa particionFinal
  where
    -- Partición inicial: estados finales vs no finales
    particionInicial = [Set.toList (estadosFinalesInt dfa), 
                       Set.toList (Set.difference (estadosInt dfa) (estadosFinalesInt dfa))]
    
    -- Refinar particiones hasta que no cambien
    particionFinal = refinarParticiones dfa (filter (not . null) particionInicial)

-- Refina particiones iterativamente
refinarParticiones :: DFAInt -> [[Int]] -> [[Int]]
refinarParticiones dfa particiones = 
  let nuevasParticiones = concatMap (refinarParticion dfa particiones) particiones
  in if nuevasParticiones == particiones
     then particiones
     else refinarParticiones dfa nuevasParticiones

-- Refina una partición individual
refinarParticion :: DFAInt -> [[Int]] -> [Int] -> [[Int]]
refinarParticion dfa todasParticiones particion =
  let grupos = agruparPorComportamiento dfa todasParticiones particion
  in if length grupos <= 1
     then [particion]
     else grupos

-- Agrupa estados por su comportamiento (a qué partición van con cada símbolo)
agruparPorComportamiento :: DFAInt -> [[Int]] -> [Int] -> [[Int]]
agruparPorComportamiento dfa particiones estados =
  let comportamientos = Map.fromList
        [(estado, calcularComportamiento dfa particiones estado) | estado <- estados]
      
      -- Agrupar estados con el mismo comportamiento
      grupos = Map.elems $ Map.fromListWith (++)
        [(comportamiento, [estado]) | (estado, comportamiento) <- Map.toList comportamientos]
  in grupos

-- Calcula el comportamiento de un estado (a qué partición va con cada símbolo)
calcularComportamiento :: DFAInt -> [[Int]] -> Int -> [Int]
calcularComportamiento dfa particiones estado =
  [encontrarParticion particiones destino
   | c <- Set.toList (alfabetoInt dfa)
   , let destino = Map.findWithDefault (-1) (estado, c) (transicionesInt dfa)
  ]

-- Encuentra en qué partición está un estado
encontrarParticion :: [[Int]] -> Int -> Int
encontrarParticion particiones estado =
  case [i | (i, p) <- zip [0..] particiones, estado `elem` p] of
    (i:_) -> i
    [] -> -1  -- Estado no encontrado

-- Construye el DFA minimizado a partir de las particiones finales
construirDFAMinimo :: DFAInt -> [[Int]] -> DFAInt
construirDFAMinimo dfa particiones = DFAInt
  { estadosInt = Set.fromList [0 .. length particiones - 1]
  , alfabetoInt = alfabetoInt dfa
  , transicionesInt = nuevasTransiciones
  , estadoInicialInt = encontrarParticion particiones (estadoInicialInt dfa)
  , estadosFinalesInt = Set.fromList
      [i | (i, p) <- zip [0..] particiones, 
       any (`Set.member` estadosFinalesInt dfa) p]
  , mapeoEstados = Map.fromList
      [(i, Set.unions [Map.findWithDefault Set.empty e (mapeoEstados dfa) | e <- p])
       | (i, p) <- zip [0..] particiones]
  }
  where
    -- Crear transiciones del DFA minimizado
    nuevasTransiciones = Map.fromList
      [ ((i, c), encontrarParticion particiones destino)
      | (i, particion) <- zip [0..] particiones
      , not (null particion)
      , let representante = head particion
      , c <- Set.toList (alfabetoInt dfa)
      , let destino = Map.findWithDefault (-1) (representante, c) (transicionesInt dfa)
      , destino /= -1
      ]
