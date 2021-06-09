{-# OPTIONS_GHC -Wall #-}

module Week4 where 
    
fun1 :: [Integer] -> Integer 
fun1 = product . map (`subtract` 2) . filter even 

fun2 :: Integer  -> Integer 
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a 
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- foldTree :: [a] -> Tree a
-- foldTree = foldr insertNode Leaf
--     where 
--         insertNode Leaf val = Node 0 Leaf val Leaf 
--         insertNode (Node depth Leaf nodeVal rightBranch) newVal =
--             Node (depth + 1) (insertNode newVal) nodeVal rightBranch
--         insertNode (Node depth leftBranch )

xor :: [Bool] -> Bool
xor = foldr (/=) False  

mapp :: (a -> b) -> [a] -> [b]
mapp f = foldr (\new lst -> f new : lst) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram =