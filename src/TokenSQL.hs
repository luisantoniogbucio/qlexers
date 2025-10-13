module TokenSQL where

-- Definición de tokens para SQL
data TokenSQL = 
  -- Palabras clave
    TSELECT
  | TFROM  
  | TWHERE
  | TINSERT
  | TINTO
  | TVALUES
  | TUPDATE
  | TSET
  | TDELETE
  | TCREATE
  | TTABLE
  | TDROP
  | TALTER
  | TAND
  | TOR
  | TNOT
  | TNULL
  | TORDER
  | TBY
  | TGROUP
  | THAVING
  | TJOIN
  | TINNER
  | TLEFT
  | TRIGHT
  | TON
  | TLIKE
  
  -- Operadores
  | TEq        -- =
  | TNeq       -- !=, <>
  | TLt        -- <
  | TGt        -- >
  | TLeqSQL    -- <=
  | TGeq       -- >=
  | TLike      -- LIKE
  
  -- Delimitadores
  | TComma     -- ,
  | TSemicolon -- ;
  | TLParen    -- (
  | TRParen    -- )
  | TAsterisk  -- *
  
  -- Literales
  | TString String    -- 'texto'
  | TNumber Int       -- 123
  | TIdentifier String -- nombres de tablas, columnas
  
  deriving (Show, Eq, Ord)
