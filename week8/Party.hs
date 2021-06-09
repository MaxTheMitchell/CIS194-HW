{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee 
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL es fun) = GL (employee:es) (fun + empFun employee)

instance Semigroup GuestList where 
    (GL es1 fun1) <> (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
    | fun1 > fun2 = gl1
    | otherwise = gl2

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f b (Node a []) = f b a
treeFold f b (Node a trees) = 
    foldl (treeFold f) (f b a) trees

-- combineGLs :: Employee -> [GuestList] -> GuestList
-- combineGLs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel _ = 
    foldr (\(gl1, gl2) (gl11, gl22) 
        -> (max gl1 gl11, max gl2 gl22))  (mempty, mempty)

maxFun :: Tree Employee -> GuestList
maxFun (Node emp []) = GL [emp] (empFun emp)
maxFun (Node boss emps) = 
     