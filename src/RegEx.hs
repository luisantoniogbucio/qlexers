module RegEx where

import Data.Set (Set)
import qualified Data.Set as Set

-- algebra de expresiones regulares sobre un alfabeto
data RegEx
  = Empty                    -- lenguaje vacio
  | Epsilon                  -- cadena vacia: {ε}
  | Char Char                -- simbolo unico: {c}
  | CharClass (Set Char)     -- clase: {a,b,c}
  | Range Char Char          -- rango: [a-z]
  | Concat RegEx RegEx       -- concatenacion: L(r) · L(s)
  | Union RegEx RegEx        -- union: L(r) ∪ L(s)
  | Star RegEx               -- cerradura de Kleene: L(r)*
  | Plus RegEx               -- cerradura positiva: L(r)+
  deriving (Show, Eq)

-- operaciones derivadas (combinadores)
digit, letter, lowercase, uppercase :: RegEx
digit     = Range '0' '9'
letter    = Union lowercase uppercase
lowercase = Range 'a' 'z'
uppercase = Range 'A' 'Z'

-- levantamiento de cadenas: String -> RegEx
-- morfismo del monoide libre (String, ·, ε) al algebra de regex
string :: String -> RegEx
string = foldr (Concat . Char) Epsilon

-- opcional: r? ≡ ε | r
optional :: RegEx -> RegEx
optional r = Union Epsilon r

-- ejemplos para imp
keyword :: String -> RegEx
keyword = string

identifier :: RegEx
identifier = Concat lowercase (Star (Union lowercase digit))

-- espacios: uno o mas caracteres de espaciado
whitespace :: RegEx
whitespace = Plus (Union (Char ' ') (Union (Char '\t') (Char '\n')))

-- numero natural: [1-9][0-9]* | 0
natural :: RegEx
natural = Union (Char '0') (Concat nonzero (Star digit))
  where nonzero = Range '1' '9'

-- numero entero: -?nat
integer :: RegEx
integer = Concat (optional (Char '-')) natural
