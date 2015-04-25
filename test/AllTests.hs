module AllTests(main) where

import ExprTests
import PresburgerTests

main = do
  allExprTests
  allPresburgerTests
