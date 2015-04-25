module Expr(Expr,
            integer,
            sym,
            plus,
            times,
            minus,
            pow,
            fraction,
            left, right,
            distributeMultiplies,
            hasNoSubExprsOfKind,
            normalizeMinuses,
            Kind(..),
            kind,
            isIntConst) where

import Data.List as L
import Test.QuickCheck

data Expr
  = Integer Integer
  | Symbol String
  | Binop String Expr Expr
  | Fraction Integer Integer
    deriving (Eq, Ord)

instance Show Expr where
  show (Integer i) = show i
  show (Symbol s) = s
  show (Binop name l r) = "(" ++ show l ++ " " ++ name ++ " " ++ show r ++ ")"
  show (Fraction l r) = "(" ++ show l ++ " / " ++ show r ++ ")"

isBinop (Binop _ _ _) = True
isBinop _ = False

integer i = Integer i
sym name = Symbol name
plus l r = Binop "+" l r
minus l r = Binop "-" l r
times l r = Binop "*" l r
pow l r = Binop "^" l r
fraction l r = Fraction l r

left (Binop _ l _) = l
right (Binop _ _ r) = r

replaceBinopOperands newL newR (Binop n _ _) = Binop n newL newR

numerator (Fraction n _) = n
denominator (Fraction _ d) = d

children :: Expr -> [Expr]
children (Binop _ l r) = [l, r]
children (Fraction l r) = [integer l, integer r]
children _ = []

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

distributeMultiplies :: Expr -> Expr
distributeMultiplies e = e

hasNoSubExprsOfKind :: Kind -> Expr -> Bool
hasNoSubExprsOfKind k e =
  case k == kind e of
    True -> False
    False -> L.and $ L.map (hasNoSubExprsOfKind k) $ children e

normalizeMinuses :: Expr -> Expr
normalizeMinuses e =
  case kind e of
    MINUS -> plus (normalizeMinuses $ left e) (times (integer (-1)) (normalizeMinuses $ right e))
    _ -> case isBinop e of
      True -> replaceBinopOperands (normalizeMinuses $ left e) (normalizeMinuses $ right e) e
      False -> e

instance Arbitrary Kind where
  arbitrary = oneof [return INTEGER, return SYMBOL, return PLUS, return MINUS, return TIMES, return POWER, return FRACTION]

instance Arbitrary Expr where
  arbitrary = sized arbitraryExprSized

arbitraryExprSized 0 = leafExpr
arbitraryExprSized n = do
  k <- arbitrary :: Gen Kind
  case k  of
    INTEGER -> do
      i <- arbitrary
      return $ integer i
    SYMBOL -> do
      n <- arbitrary
      return $ sym n
    PLUS -> do
      l <- arbitraryExprSized $ reduceSize n
      r <- arbitraryExprSized $ reduceSize n
      return $ plus l r
    MINUS -> do
      l <- arbitraryExprSized $ reduceSize n
      r <- arbitraryExprSized $ reduceSize n 
      return $ minus l r
    TIMES -> do
      l <- arbitraryExprSized $ reduceSize n
      r <- arbitraryExprSized $ reduceSize n
      return $ times l r
    POWER -> do
      l <- arbitraryExprSized $ reduceSize n
      r <- arbitraryExprSized $ reduceSize n
      return $ pow l r
    FRACTION -> do
      l <- arbitrary
      r <- arbitrary
      return $ fraction l r
    
      
leafExpr =
  oneof [arbitrary >>= (return . integer),
         arbitrary >>= (return . sym)]

reduceSize n = div n 2
