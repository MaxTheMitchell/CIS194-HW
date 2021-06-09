{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where 

import Data.List (unfoldr)

fib :: Integer -> Integer
fib 0 = 0 
fib 1 = 1
fib number = fib (number - 1) + fib (number - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

fibs3 :: [Integer]
fibs3 = unfoldr (\(a,b) -> Just(a, (b, a+b))) (0, 1)

data Stream a
    = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a stream) = a : streamToList stream

instance Show a => Show (Stream a) where
    show =  unwords . map show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a 

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Cons a stream) =
    Cons (func a) (streamMap func stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed func start =
    Cons start (streamFromSeed func (func start))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamMap rule $ streamFromSeed (+ 1) 1

rule :: Integer -> Integer 
rule numb = toInteger . length . takeWhile (isEvenDiv numb) $ iterate (*2) 2

isEvenDiv :: Integer -> Integer -> Bool 
isEvenDiv num demon = 
    (num `div` demon) * demon == num

instance Num (Stream Integer) where
    fromInteger i = Cons i $ streamRepeat 0
    negate (Cons i stream) = Cons (negate i) (negate stream)
    (+) (Cons i1 stream1) (Cons i2 stream2) = Cons (i1 + i2) (stream1 + stream2)
    (*) (Cons a0 as) stream@(Cons b0 bs) = Cons (a0*b0) (streamMap (*a0) bs + as * stream) 

data Matrix 
    = Matrix Integer Integer Integer Integer 

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 i = (\(Matrix _ n _ _) -> n) $ Matrix 1 1 1 0 ^ i