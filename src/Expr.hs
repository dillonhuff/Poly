module Expr(integer,
            sym,
            plus,
            times,
            minus,
            pow,
            Kind(..),
            kind) where

data Expr
  = Integer Integer
  | Symbol String
  | Binop String Expr Expr
    deriving (Eq, Ord, Show)

integer i = Integer i
sym name = Symbol name
plus l r = Binop "+" l r
minus l r = Binop "-" l r
times l r = Binop "*" l r
pow l r = Binop "^" l r

data Kind
  = INTEGER
  | SYMBOL
  | PLUS
  | TIMES
  | MINUS
  | POWER
    deriving (Eq, Ord, Show)

kind :: Expr -> Kind
kind (Integer _) = INTEGER
kind (Symbol _) = SYMBOL
kind (Binop "+" _ _) = PLUS
kind (Binop "*" _ _) = TIMES
kind (Binop "-" _ _) = MINUS
kind (Binop "^" _ _) = POWER
