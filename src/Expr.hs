module Expr(Expr,
            integer,
            sym,
            plus,
            times,
            minus,
            pow,
            fraction,
            left, right,
            Kind(..),
            kind,
            isIntConst) where

data Expr
  = Integer Integer
  | Symbol String
  | Binop String Expr Expr
  | Fraction Integer Integer
    deriving (Eq, Ord, Show)

integer i = Integer i
sym name = Symbol name
plus l r = Binop "+" l r
minus l r = Binop "-" l r
times l r = Binop "*" l r
pow l r = Binop "^" l r
fraction l r = Fraction l r

left (Binop _ l _) = l
right (Binop _ _ r) = r

data Kind
  = INTEGER
  | SYMBOL
  | PLUS
  | TIMES
  | MINUS
  | POWER
  | FRACTION
    deriving (Eq, Ord, Show)

kind :: Expr -> Kind
kind (Integer _) = INTEGER
kind (Symbol _) = SYMBOL
kind (Binop "+" _ _) = PLUS
kind (Binop "*" _ _) = TIMES
kind (Binop "-" _ _) = MINUS
kind (Binop "^" _ _) = POWER
kind (Fraction _ _) = FRACTION

isIntConst :: Expr -> Bool
isIntConst (Integer _) = True
isIntConst (Binop _ l r) = isIntConst l && isIntConst r
isIntConst _ = False
