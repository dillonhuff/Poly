module ExprTests(allExprTests) where

import Test.QuickCheck

import Expr
import TestUtils

allExprTests = do
  testFunction kind kindCases
  testFunction isIntConst intConstCases
  invariantTests

kindCases =
  [(integer 12, INTEGER),
   (sym "a", SYMBOL),
   (plus (sym "a") (sym "b"), PLUS),
   (times (sym "a") (integer 4), TIMES),
   (minus (integer 12) (integer 4), MINUS),
   (pow (integer 12) (integer 2), POWER),
   (fraction 12 3, FRACTION)]

intConstCases =
  [(integer 45, True),
   (plus (integer 3) (integer 6), True)]

invariantTests = do
  quickCheck noMinusesAfterNormalizingMinuses

noMinusesAfterNormalizingMinuses e =
  hasNoSubExprsOfKind MINUS $ normalizeMinuses e
