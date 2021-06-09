{-# OPTIONS_GHC -Wall #-}

module Calc06 where 


class HasVars a where
    var :: String -> a

data VarExprT 
    = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    | Var String 

instance HasVars VarExprT where 
    var = Var 

instance Expr VarExprT where 
    lit = Calc.Lit
    add = Calc.Add
    mul = Calc.Mul
