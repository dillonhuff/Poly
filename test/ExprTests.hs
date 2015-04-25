module ExprTests(allExprTests) where

import Expr
import TestUtils

allExprTests = do
  testFunction kind kindCases

kindCases =
  [(integer 12, INTEGER),
   (sym "a", SYMBOL),
   (plus (sym "a") (sym "b"), PLUS),
   (times (sym "a") (integer 4), TIMES),
   (minus (integer 12) (integer 4), MINUS),
   (pow (integer 12) (integer 2), POWER)]
