module Presburger(isPresburgerExpr,
                  toPresburgerExpr,
                  presburgerExpr) where

import Data.Map as M

import Expr

isPresburgerExpr :: Expr -> Bool
isPresburgerExpr e =
  case isPresburgerMonomial e of
    True -> True
    False -> case kind e of
      PLUS -> isPresburgerExpr (left e) && isPresburgerExpr (right e)
      MINUS -> isPresburgerExpr (left e) && isPresburgerExpr (right e)
      TIMES -> (isIntConst (left e) && isPresburgerExpr (right e))
            || (isPresburgerExpr (left e) && isIntConst (right e))
      _ -> False

isPresburgerMonomial :: Expr -> Bool
isPresburgerMonomial e =
  case kind e of
    INTEGER -> True
    SYMBOL -> True
    TIMES -> ((isIntConst (left e)) && (kind (right e) == SYMBOL))
          || ((kind (left e) == SYMBOL) && (isIntConst (right e)))
          || ((isIntConst (left e)) && (isIntConst (right e)))
    _ -> False

data PresburgerExpr
  = PresburgerExpr (Map String Integer) Integer
    deriving (Eq, Ord, Show)

presburgerExpr :: [(String, Integer)] -> Integer -> PresburgerExpr
presburgerExpr coeffMap constVal = PresburgerExpr (M.fromList coeffMap) constVal

toPresburgerExpr :: Expr -> PresburgerExpr
toPresburgerExpr e = presburgerExpr [] 12
