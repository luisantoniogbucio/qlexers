module RegEx where

import Data.Set (Set)
import qualified Data.Set as Set

-- Regular Expression algebraic data type
data RegEx
  = Empty                    -- ε (epsilon - empty string)
  | Epsilon                  -- explicit epsilon (alternative naming)
  | Char Char                -- single character
  | CharClass (Set Char)     -- character class [abc] = {a,b,c}
  | Range Char Char          -- character range [a-z]
  | Concat RegEx RegEx       -- concatenation: r1 · r2
  | Union RegEx RegEx        -- alternation: r1 | r2
  | Star RegEx               -- Kleene star: r*
  | Plus RegEx               -- one or more: r+
  deriving (Show, Eq)

-- Smart constructors for common patterns
digit :: RegEx
digit = Range '0' '9'

letter :: RegEx
letter = Union (Range 'a' 'z') (Range 'A' 'Z')

lowercase :: RegEx
lowercase = Range 'a' 'z'

-- Helper: string to RegEx
string :: String -> RegEx
string [] = Empty
string [c] = Char c
string (c:cs) = Concat (Char c) (string cs)

-- Helper: optional r?
optional :: RegEx -> RegEx
optional r = Union r Empty

-- Examples for IMP tokens (we'll use these later)
-- keyword "skip" = string "skip"
-- identifier = Concat lowercase (Star (Union lowercase digit))
