module RegExp where

import Data.Set (Set)
import qualified Data.Set as Set

-- Definición de expresiones regulares
data RegExp
  = Empty                    -- Cadena vacía
  | Char Char               -- Un solo carácter
  | CharSet (Set Char)      -- Conjunto de caracteres [abc]
  | Concat RegExp RegExp    -- Concatenación r1·r2
  | Union RegExp RegExp     -- Unión r1|r2
  | Star RegExp             -- Cerradura de Kleene r*
  deriving (Show, Eq)

-- Convierte una cadena en expresión regular
cadena :: String -> RegExp
cadena [] = Empty
cadena [c] = Char c
cadena (c:cs) = Concat (Char c) (cadena cs)

-- Crea conjunto de caracteres específicos
cualquieraDe :: String -> RegExp
cualquieraDe chars = CharSet (Set.fromList chars)

-- Crea rango de caracteres consecutivos
rango :: Char -> Char -> RegExp
rango low high = CharSet (Set.fromList [low..high])

-- Patrones comunes
digito :: RegExp
digito = rango '0' '9'

letra :: RegExp
letra = Union (rango 'a' 'z') (rango 'A' 'Z')

-- Una o más repeticiones (r+)
unoOMas :: RegExp -> RegExp
unoOMas r = Concat r (Star r)

-- Opcional (r?)
opcional :: RegExp -> RegExp
opcional r = Union r Empty
