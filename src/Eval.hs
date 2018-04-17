module Eval where

import           Data.Maybe
import           Parse

unReal f = FuncVal (RealVal . f . unwrapReal)

type Env = [(String, Val)]

eval :: Env -> Expr -> Val
eval env (Const x)            = x
eval env (Var "sin")          = unReal sin
eval env (Var "cos")          = unReal cos
eval env (Var "exp")          = unReal exp
eval env (Var "log")          = unReal log
eval env (Var v)              = fromJust (lookup v env)
eval env (Eq  a b)            = BoolVal (eval env a == eval env b)
eval env (Mul a b)            = RealVal (evalReal env a * evalReal env b)
eval env (Div a b)            = RealVal (evalReal env a / evalReal env b)
eval env (Add a b)            = RealVal (evalReal env a + evalReal env b)
eval env (Sub a b)            = RealVal (evalReal env a - evalReal env b)
eval env (App f e)            = evalFunc env f (eval env e)
eval env (If pred conseq alt) = if evalBool env pred then eval env conseq else eval env alt
eval env (Let bnds body)      = let env' = fmap (\(b, v) -> (b, eval env' v)) bnds ++ env
                                in eval env' body
eval env (Lambda param body)  = FuncVal (\v -> eval ((param, v):env) body)

unwrapReal :: Val -> Double
unwrapReal (RealVal x) = x
unwrapReal _           = error "Type error. Expected real."

unwrapBool :: Val -> Bool
unwrapBool (BoolVal x) = x
unwrapBool _           = error "Type error. Expected bool."

unwrapFunc :: Val -> (Val -> Val)
unwrapFunc (FuncVal x) = x
unwrapFunc _           = error "Type error. Expected func."

evalReal :: Env -> Expr -> Double
evalReal env = unwrapReal . eval env

evalBool :: Env -> Expr -> Bool
evalBool env = unwrapBool . eval env

evalFunc :: Env -> Expr -> (Val -> Val)
evalFunc env = unwrapFunc . eval env
