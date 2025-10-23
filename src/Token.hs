module Token where

-- tipos de token para el lenguaje imp
data TokenType
  = TSkip | TIf | TThen | TElse | TWhile | TDo     -- keywords
  | TTrue | TFalse | TNot | TAnd                    -- boolean ops
  | TId String | TNum Int                           -- identifiers & literals
  | TPlus | TMinus | TTimes | TEq | TLeq | TAssign  -- operators
  | TSemi | TLParen | TRParen                       -- delimiters
  deriving (Show, Eq, Ord)

-- token con informacion de posicion
data Token = Token
  { tokenType :: TokenType
  , tokenPos  :: Int
  } deriving (Show, Eq)

-- constructor: alias para Token
mkToken :: TokenType -> Int -> Token
mkToken = Token
