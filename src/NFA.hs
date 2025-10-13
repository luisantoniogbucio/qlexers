module NFA where

import RegExp
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- Autómata finito no determinista con transiciones epsilon
data NFA = NFA
  { estados :: Set Int                              -- Conjunto de estados
  , alfabeto :: Set Char                           -- Alfabeto de entrada
  , transiciones :: Map (Int, Maybe Char) (Set Int) -- Función de transición
  , estadoInicial :: Int                           -- Estado inicial
  , estadosFinales :: Set Int                      -- Estados de aceptación
  } deriving (Show, Eq)

-- Contador para generar estados únicos
type ContadorEstados = Int

-- Construcción de NFA a partir de expresión regular
construirNFA :: RegExp -> NFA
construirNFA regex = nfa
  where
    (nfa, _) = construirConContador regex 0

-- Función interna que mantiene el contador de estados
construirConContador :: RegExp -> ContadorEstados -> (NFA, ContadorEstados)

-- Caso: cadena vacía
construirConContador Empty contador = 
  let inicial = contador
      final = contador + 1
      estados = Set.fromList [inicial, final]
      trans = Map.singleton (inicial, Nothing) (Set.singleton final)
  in (NFA estados Set.empty trans inicial (Set.singleton final), contador + 2)

-- Caso: carácter simple
construirConContador (Char c) contador =
  let inicial = contador
      final = contador + 1
      estados = Set.fromList [inicial, final]
      alfabeto = Set.singleton c
      trans = Map.singleton (inicial, Just c) (Set.singleton final)
  in (NFA estados alfabeto trans inicial (Set.singleton final), contador + 2)

-- Caso: conjunto de caracteres
construirConContador (CharSet chars) contador =
  let inicial = contador
      final = contador + 1
      estados = Set.fromList [inicial, final]
      alfabeto = chars
      -- Crear transición para cada carácter del conjunto
      trans = Map.fromList [((inicial, Just c), Set.singleton final) | c <- Set.toList chars]
  in (NFA estados alfabeto trans inicial (Set.singleton final), contador + 2)

-- Caso: concatenación r1·r2
construirConContador (Concat r1 r2) contador =
  let (nfa1, contador1) = construirConContador r1 contador
      (nfa2, contador2) = construirConContador r2 contador1
      
      -- Unir estados y alfabetos
      estadosUnidos = Set.union (estados nfa1) (estados nfa2)
      alfabetoUnido = Set.union (alfabeto nfa1) (alfabeto nfa2)
      
      -- Conectar estados finales de nfa1 con inicial de nfa2 usando epsilon
      epsilonTrans = Map.fromList 
        [((final, Nothing), Set.singleton (estadoInicial nfa2)) 
         | final <- Set.toList (estadosFinales nfa1)]
      
      -- Unir todas las transiciones
      trans = Map.unionWith Set.union (transiciones nfa1) 
              (Map.unionWith Set.union epsilonTrans (transiciones nfa2))
      
  in (NFA estadosUnidos alfabetoUnido trans (estadoInicial nfa1) (estadosFinales nfa2), contador2)

-- Caso: unión r1|r2
construirConContador (Union r1 r2) contador =
  let (nfa1, contador1) = construirConContador r1 (contador + 2)
      (nfa2, contador2) = construirConContador r2 contador1
      
      nuevoInicial = contador
      nuevoFinal = contador + 1
      
      -- Unir todos los estados
      estadosUnidos = Set.unions [estados nfa1, estados nfa2, 
                           Set.fromList [nuevoInicial, nuevoFinal]]
      
      alfabetoUnido = Set.union (alfabeto nfa1) (alfabeto nfa2)
      
      -- Epsilon desde nuevo inicial a ambos NFAs
      transInicial = Map.singleton (nuevoInicial, Nothing) 
                     (Set.fromList [estadoInicial nfa1, estadoInicial nfa2])
      
      -- Epsilon desde estados finales al nuevo final
      transFinal1 = Map.fromList 
        [((final, Nothing), Set.singleton nuevoFinal) 
         | final <- Set.toList (estadosFinales nfa1)]
      transFinal2 = Map.fromList 
        [((final, Nothing), Set.singleton nuevoFinal) 
         | final <- Set.toList (estadosFinales nfa2)]
      
      -- Unir todas las transiciones
      trans = Map.unionsWith Set.union 
              [transiciones nfa1, transiciones nfa2, 
               transInicial, transFinal1, transFinal2]
      
  in (NFA estadosUnidos alfabetoUnido trans nuevoInicial (Set.singleton nuevoFinal), contador2)

-- Caso: cerradura de Kleene r*
construirConContador (Star r) contador =
  let (nfa, contador1) = construirConContador r (contador + 2)
      
      nuevoInicial = contador
      nuevoFinal = contador + 1
      
      estadosUnidos = Set.union (estados nfa) (Set.fromList [nuevoInicial, nuevoFinal])
      alfabetoUnido = alfabeto nfa
      
      -- Epsilon desde nuevo inicial a inicial del NFA y al final (para cadena vacía)
      transInicial = Map.singleton (nuevoInicial, Nothing) 
                     (Set.fromList [estadoInicial nfa, nuevoFinal])
      
      -- Epsilon desde finales de vuelta al inicial (repetición) y al nuevo final
      transLoop = Map.fromList 
        [((final, Nothing), Set.fromList [estadoInicial nfa, nuevoFinal]) 
         | final <- Set.toList (estadosFinales nfa)]
      
      trans = Map.unionsWith Set.union 
              [transiciones nfa, transInicial, transLoop]
      
  in (NFA estadosUnidos alfabetoUnido trans nuevoInicial (Set.singleton nuevoFinal), contador1)

-- Calcula la cerradura epsilon de un estado
cerraduraEpsilon :: NFA -> Int -> Set Int
cerraduraEpsilon nfa estado = buscar (Set.singleton estado) (Set.singleton estado)
  where
    buscar visitados frontera
      | Set.null frontera = visitados
      | otherwise =
          let nuevos = Set.unions 
                [Map.findWithDefault Set.empty (s, Nothing) (transiciones nfa) 
                 | s <- Set.toList frontera]
              noVisitados = Set.difference nuevos visitados
          in buscar (Set.union visitados noVisitados) noVisitados

-- Cerradura epsilon para conjunto de estados
cerraduraEpsilonConjunto :: NFA -> Set Int -> Set Int
cerraduraEpsilonConjunto nfa estados = 
  Set.unions [cerraduraEpsilon nfa s | s <- Set.toList estados]
