module Lib where

import           Eval
import           Parse

interpret :: String -> Val
interpret = eval [] . parseExpr

p0 = " let fib = lam n ->                        \
     \       if n = 0                            \
     \       then 0                              \
     \       else if n = 1                       \
     \            then 1                         \
     \            else fib (n - 1) + fib (n - 2) \
     \ in fib 10                                 "

p1 = "let add = lam a b -> a + b in add 1300 37"

p2 = " let even = lam n ->                         \
     \       if n = 0 then true  else odd  (n - 1) \
     \   , odd  = lam n ->                         \
     \       if n = 0 then false else even (n - 1) \
     \ in even                                     "
even' = unwrapFunc (interpret p2)
-- even' (RealVal 10) -- -> True
-- even' (RealVal 11) -- -> False

