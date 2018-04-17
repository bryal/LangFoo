module Eval where

import           Parse

subst :: String -> Val -> Expr -> Expr
subst b v (Var b') | b == b' = Const v
subst b v (Eq  x y) = Eq  (subst b v x) (subst b v y)
subst b v (Mul x y) = Mul (subst b v x) (subst b v y)
subst b v (Div x y) = Div (subst b v x) (subst b v y)
subst b v (Add x y) = Add (subst b v x) (subst b v y)
subst b v (Sub x y) = Sub (subst b v x) (subst b v y)
subst b v (App f e) = App f (subst b v e)
subst b v (If pred conseq alt)    = If (subst b v pred) (subst b v conseq) (subst b v alt)
subst b v (Let b' v' e) | b /= b' = (Let b' v' (subst b v e))
subst _ _ e = e

eval :: Expr -> Val
eval (Const x)            = x
eval (Eq  a b)            = BoolVal (eval a == eval b)
eval (Mul a b)            = RealVal (evalReal a * evalReal b)
eval (Div a b)            = RealVal (evalReal a / evalReal b)
eval (Add a b)            = RealVal (evalReal a + evalReal b)
eval (Sub a b)            = RealVal (evalReal a - evalReal b)
eval (App Sin e)          = RealVal (sin (evalReal e))
eval (App Cos e)          = RealVal (cos (evalReal e))
eval (App Exp e)          = RealVal (exp (evalReal e))
eval (App Log e)          = RealVal (log (evalReal e))
eval (If pred conseq alt) = if evalBool pred then eval conseq else eval alt
eval (Let bnd val body)   = eval (subst bnd (eval val) body)

evalReal :: Expr -> Double
evalReal e = case eval e of
  RealVal x -> x
  _         -> error "Type error. Expected real."

evalBool :: Expr -> Bool
evalBool e = case eval e of
  BoolVal b -> b
  _         -> error "Type error. Expected bool."
