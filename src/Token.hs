module Token where

data Token
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
  | TId String
  | TNum Int
  | TPlus
  | TMinus
  | TTimes
  | TEq
  | TLeq
  | TAssign
  | TSemi
  | TLParen
  | TRParen
  deriving (Show, Eq, Ord)
