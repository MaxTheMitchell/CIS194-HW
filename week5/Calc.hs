{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where 

import ExprT
import Parser
import qualified Data.Map as M


newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
    lit :: Integer -> a 
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where 
    lit = ExprT.Lit 
    add = ExprT.Add
    mul = ExprT.Mul 

instance Expr Integer where 
    lit = id 
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||) 
    mul = (&&)  

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax $ max a b 
    mul (MinMax a) (MinMax b) = MinMax $ min a b
    
instance Expr Mod7 where 
    lit = Mod7
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr str = 
    case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of  
        (Just expr) -> Just $ eval expr 
        Nothing -> Nothing 


class HasVars a where
    var :: String -> a

data VarExprT 
    = Lit Integer
    | Add VarExprT VarExprT
    | Mul VarExprT VarExprT
    | Var String 
    deriving Show 

instance HasVars VarExprT where 
    var = Var 

instance Expr VarExprT where 
    lit = Calc.Lit
    add = Calc.Add
    mul = Calc.Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit i _ = Just i
    add 


