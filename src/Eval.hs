module Eval where

import           Data.Maybe
import           Parse

arithmUnopToVal  f  = FuncVal (RealVal . f . unwrapReal)
arithmBinopToVal op = FuncVal (arithmUnopToVal . op . unwrapReal)

type Env = [(String, Val)]

eval :: Env -> Expr -> Val
eval env (Const x)            = x
eval env (Var "sin")          = arithmUnopToVal sin
eval env (Var "cos")          = arithmUnopToVal cos
eval env (Var "exp")          = arithmUnopToVal exp
eval env (Var "log")          = arithmUnopToVal log
eval env (Var "=")            = FuncVal (\a -> FuncVal (BoolVal . (==a)))
eval env (Var "*")            = arithmBinopToVal (*)
eval env (Var "/")            = arithmBinopToVal (/)
eval env (Var "+")            = arithmBinopToVal (+)
eval env (Var "-")            = arithmBinopToVal (-)
eval env (Var v)              = fromJust (lookup v env)
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
