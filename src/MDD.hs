module MDD where

import Minimizar
import Token
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

-- Estructura de datos para matching eficiente
data MDD = MDD
  { nodos :: Map Int Nodo                          -- Nodos del MDD
  , raiz :: Int                                    -- Nodo raíz
  } deriving (Show, Eq)

-- Nodo del MDD
data Nodo = Nodo
  { esAceptacion :: Bool                           -- Si es estado de aceptación
  , tipoToken :: Maybe Token                       -- Tipo de token si es aceptación
  , transicionesNodo :: Map Char Int               -- Transiciones a otros nodos
  } deriving (Show, Eq)

-- Convierte DFA minimizado a MDD
dfaAMDD :: DFAInt -> Map Int Token -> MDD
dfaAMDD dfa mapeoTokens = MDD
  { nodos = Map.fromList [(i, crearNodo i) | i <- Set.toList (estadosInt dfa)]
  , raiz = estadoInicialInt dfa
  }
  where
    crearNodo :: Int -> Nodo
    crearNodo estado = Nodo
      { esAceptacion = estado `Set.member` estadosFinalesInt dfa
      , tipoToken = Map.lookup estado mapeoTokens
      , transicionesNodo = Map.fromList
          [ (c, dest)
          | c <- Set.toList (alfabetoInt dfa)
          , let destino = Map.lookup (estado, c) (transicionesInt dfa)
          , destino /= Nothing
          , let Just dest = destino
          ]
      }

-- Ejecuta el MDD sobre una cadena de entrada
ejecutarMDD :: MDD -> String -> Maybe (Token, String, String)
ejecutarMDD mdd entrada = buscarCoincidencia mdd entrada (raiz mdd) "" Nothing

-- Busca la coincidencia más larga
buscarCoincidencia :: MDD -> String -> Int -> String -> Maybe (Token, String, String) -> Maybe (Token, String, String)
buscarCoincidencia mdd entrada estadoActual coincidenciaActual mejorCoincidencia =
  case Map.lookup estadoActual (nodos mdd) of
    Nothing -> mejorCoincidencia
    Just nodo ->
      let -- Actualizar mejor coincidencia si este es un estado de aceptación
          nuevaMejor = if esAceptacion nodo
                      then case tipoToken nodo of
                             Just token -> Just (token, entrada, coincidenciaActual)
                             Nothing -> mejorCoincidencia
                      else mejorCoincidencia
      in case entrada of
           [] -> nuevaMejor
           (c:resto) ->
             case Map.lookup c (transicionesNodo nodo) of
               Nothing -> nuevaMejor
               Just siguienteEstado -> 
                 buscarCoincidencia mdd resto siguienteEstado 
                                   (coincidenciaActual ++ [c]) nuevaMejor

-- Tokeniza una cadena completa usando el MDD
tokenizar :: MDD -> String -> [(Token, String)]
tokenizar mdd = tokenizarAux []
  where
    tokenizarAux tokens [] = reverse tokens
    tokenizarAux tokens entrada@(c:resto) =
      case ejecutarMDD mdd entrada of
        Just (token, restante, texto) -> tokenizarAux ((token, texto):tokens) restante
        Nothing -> 
          -- Saltar caracteres no reconocidos (espacios, etc.)
          if c `elem` " \t\n\r"
          then tokenizarAux tokens resto
          else error $ "Carácter no reconocido: " ++ [c]
