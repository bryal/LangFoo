module Eval where

import           Parse

eval :: Expr -> Double
eval (Const x)   = x
eval (Mul a b)   = eval a * eval b
eval (Div a b)   = eval a / eval b
eval (Add a b)   = eval a + eval b
eval (Sub a b)   = eval a - eval b
eval (App Sin e) = sin (eval e)
eval (App Cos e) = cos (eval e)
eval (App Exp e) = exp (eval e)
eval (App Log e) = log (eval e)
