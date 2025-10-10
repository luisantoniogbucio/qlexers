module Token where

-- Token types for IMP language
data TokenType
  -- Keywords
  = TSkip
  | TIf
  | TThen
  | TElse
  | TWhile
  | TDo
  | TTrue
  | TFalse
  | TNot
  | TAnd
  -- Identifiers and literals
  | TId String
  | TNum Int
  -- Operators
  | TPlus
  | TMinus
  | TTimes
  | TEq
  | TLeq
  | TAssign
  -- Delimiters
  | TSemi
  | TLParen
  | TRParen
  deriving (Show, Eq, Ord)  -- Add Ord here

-- Token with position information (optional but useful)
data Token = Token
  { tokenType :: TokenType
  , tokenPos  :: Int  -- character position in input
  } deriving (Show, Eq)

-- Helper constructor
mkToken :: TokenType -> Int -> Token
mkToken = Token
