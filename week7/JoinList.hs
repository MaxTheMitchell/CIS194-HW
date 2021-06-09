{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module JoinList where 

import Sized
import Scrabble
import Buffer

data JoinList m a 
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

instance Semigroup m => Semigroup (JoinList m a) where
    Empty <> jl = jl
    jl <> Empty = jl
    l@(Append m1 _ _) <> r@(Append m2 _ _) = Append (m1 <> m2) l r
    l@(Append m1 _ _) <> r@(Single m2 _) = Append (m1 <> m2) l r 
    l@(Single m1 _) <> r@(Append m2 _ _) = Append (m1 <> m2) l r 
    l@(Single m1 _) <> r@(Single m2 _) = Append (m1 <> m2) l r 

instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty 
    mappend = (<>)

instance (Sized b, Monoid b) => Sized (JoinList b a) where
    size Empty = 0 
    size (Single m _) = size m 
    size (Append m _ _) = size m 

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a 
(+++) = (<>)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0 = Nothing 
indexJ _ Empty = Nothing 
indexJ 0 (Single _ n) = Just n
indexJ _ (Single _ _) = Nothing 
indexJ n (Append _ l r) 
    | n < len l = indexJ n l
    | otherwise = indexJ (n - len l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl  
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append sz l r) 
    | getSize (size sz) <= n = Empty 
    | len l < n = dropJ (n - len l) r 
    | otherwise = dropJ n l <> r 

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ jl@(Single _ _) = jl 
takeJ n jl@(Append sz l r)
    | getSize (size sz) <= n = jl
    | len l > n = takeJ n l
    | otherwise = l <> takeJ (n - len l) r


len :: (Sized b, Monoid b) => JoinList b a -> Int 
len = getSize . size

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

---------------------------------------------------------------

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

instance Buffer (JoinList (Score, Size) String) where 
    toString = show 
    fromString str = Single (scoreString str, Size 1) str
    line = indexJ
    replaceLine i str jl 
        | len jl >= i = jl 
        | otherwise =
            takeJ (i-1) jl 
            +++ fromString str 
            +++ dropJ (i+1) jl 
    numLines = len 
    value Empty = 0 
    value (Single (Score i, _) _) = i
    value (Append (Score i, _) _ _) = i 