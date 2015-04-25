module PresburgerTests(allPresburgerTests) where

import Expr
import Presburger
import TestUtils

allPresburgerTests = do
  testFunction isPresburgerExpr presburgerCases
  testFunction toPresburgerExpr presburgerExprCases

presburgerCases =
  [(integer 4, True),
   (sym "nope", True),
   (fraction 12 3, False),
   (times (integer 4) (integer 324), True),
   (times (integer 3) (sym "w"), True),
   (times (sym "q") (integer 234), True),
   (plus (sym "a") (integer 987), True),
   (plus (sym "a") (times (sym "a") (sym "b")), False),
   (minus (sym "a") (integer 53), True),
   (minus (sym "b") (sym "a"), True),
   (plus (minus (sym "a") (sym "b")) (plus (times (integer 2) (sym "a")) (sym "d")), True),
   (plus (minus (sym "a") (sym "b")) (plus (times (sym "a") (sym "a")) (sym "d")), False),
   (times (integer 43) (plus (integer 3) (sym "a")), True)]

presburgerExprCases =
  [(integer 12, presburgerExpr [] 12),
   (integer 15, presburgerExpr [] 15)]
