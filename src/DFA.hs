module DFA where

import NFA
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')

-- Autómata finito determinista
data DFA = DFA
  { estadosDFA :: Set (Set Int)                    -- Estados (conjuntos de estados del NFA)
  , alfabetoDFA :: Set Char                        -- Alfabeto
  , transicionesDFA :: Map (Set Int, Char) (Set Int) -- Transiciones deterministas
  , estadoInicialDFA :: Set Int                    -- Estado inicial
  , estadosFinalesDFA :: Set (Set Int)             -- Estados finales
  } deriving (Show, Eq)

-- Convierte NFA a DFA usando construcción de subconjuntos
nfaADFA :: NFA -> DFA
nfaADFA nfa = DFA
  { estadosDFA = estadosGenerados
  , alfabetoDFA = alfabeto nfa
  , transicionesDFA = transicionesGeneradas
  , estadoInicialDFA = estadoInicialDFA
  , estadosFinalesDFA = estadosAceptacion
  }
  where
    -- Estado inicial del DFA es la cerradura epsilon del estado inicial del NFA
    estadoInicialDFA = cerraduraEpsilon nfa (estadoInicial nfa)
    
    -- Construir estados y transiciones usando algoritmo de lista de trabajo
    (estadosGenerados, transicionesGeneradas) = 
      construirEstados (Set.singleton estadoInicialDFA) estadoInicialDFA Map.empty
    
    -- Estados de aceptación: aquellos que contienen algún estado final del NFA
    estadosAceptacion = Set.filter
      (\estado -> not $ Set.null $ Set.intersection estado (estadosFinales nfa))
      estadosGenerados
    
    -- Algoritmo de lista de trabajo para explorar todos los estados alcanzables
    construirEstados :: Set (Set Int) -> Set Int -> Map (Set Int, Char) (Set Int) 
                     -> (Set (Set Int), Map (Set Int, Char) (Set Int))
    construirEstados visitados listaTrabajo transiciones
      | Set.null listaTrabajo = (visitados, transiciones)
      | otherwise = 
          let -- Procesar todos los caracteres para el estado actual
              estadoActual = listaTrabajo
              nuevasTransYEstados = 
                [ ((estadoActual, c), destino)
                | c <- Set.toList (alfabeto nfa)
                , let destino = calcularDestino estadoActual c
                , not (Set.null destino)
                ]
              
              nuevasTransiciones = Map.fromList nuevasTransYEstados
              nuevosEstados = Set.fromList $ map snd nuevasTransYEstados
              noVisitados = Set.difference nuevosEstados visitados
              
              todasTransiciones = Map.union transiciones nuevasTransiciones
              todosVisitados = Set.union visitados noVisitados
              
          in if Set.null noVisitados
             then (todosVisitados, todasTransiciones)
             else foldl' (\(v, t) estado -> construirEstados v estado t) 
                         (todosVisitados, todasTransiciones) 
                         (Set.toList noVisitados)
    
    -- Calcula el estado destino para un estado y carácter dados
    calcularDestino :: Set Int -> Char -> Set Int
    calcularDestino estado c = 
      let -- Estados alcanzables directamente con el carácter c
          alcanzables = Set.unions
            [ Map.findWithDefault Set.empty (s, Just c) (transiciones nfa)
            | s <- Set.toList estado
            ]
      in cerraduraEpsilonConjunto nfa alcanzables
