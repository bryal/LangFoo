module Lib (interpret) where

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

-- fibonacci sequence using lazy lists
p3 = " let cons = lam a b get_car -> if get_car then a else b             \
     \   , car  = lam cons -> cons true                                   \
     \   , cdr  = lam cons -> cons false                                  \
     \   , nth  = lam n l -> if n = 0 then car l else nth (n - 1) (cdr l) \
     \   , or   = lam a b -> if a then true else b                        \
     \   , zipWith = lam f l1 l2 ->                                       \
     \       if or (car l1 = nil) (car l2 = nil)                          \
     \       then nil                                                     \
     \       else cons (f (car l1) (car l2))                              \
     \                 (zipWith f (cdr l1) (cdr l2))                      \
     \   , fibs = cons 0 (cons 1 (zipWith (+) fibs (cdr fibs)))           \
     \ in lam n -> nth n fibs                                             "
fib n = unwrapFunc (interpret p3) (RealVal n)

p4 = "let plus = (+), a = 1, b = 4 in a `plus` b"

