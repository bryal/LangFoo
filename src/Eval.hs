module Eval where

import           Parse

data Val = RealVal Double
         | BoolVal Bool
  deriving Eq

instance Show Val where
  show (RealVal x) = show x
  show (BoolVal b) = show b

eval :: Expr -> Val
eval (Const x)   = RealVal x
eval (Eq  a b)   = BoolVal (eval a == eval b)
eval (Mul a b)   = RealVal (evalReal a * evalReal b)
eval (Div a b)   = RealVal (evalReal a / evalReal b)
eval (Add a b)   = RealVal (evalReal a + evalReal b)
eval (Sub a b)   = RealVal (evalReal a - evalReal b)
eval (App Sin e) = RealVal (sin (evalReal e))
eval (App Cos e) = RealVal (cos (evalReal e))
eval (App Exp e) = RealVal (exp (evalReal e))
eval (App Log e) = RealVal (log (evalReal e))
eval (If p c a)  = if evalBool p then eval c else eval a

evalReal :: Expr -> Double
evalReal e = case eval e of
  RealVal x -> x
  _         -> error "Type error. Expected real."

evalBool :: Expr -> Bool
evalBool e = case eval e of
  BoolVal b -> b
  _         -> error "Type error. Expected bool."
