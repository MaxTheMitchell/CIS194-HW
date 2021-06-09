{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module Golf where

import Data.List

skips :: [a] -> [[a]]
skips lst = 
    map 
        (\interval -> [v |  (v, i) <- zip lst [1..], i `mod` interval == 0]) 
        [1..length lst]

localMaxima :: [Integer] -> [Integer]
localMaxima (left:mid:right:lst) =
    [mid | left < mid && mid > right] ++ localMaxima (mid:right:lst)
localMaxima _ = []

histogram :: [Integer] -> String
histogram =
    (\str -> "\n" ++ str ++ "\n==========\n0123456789\n")
    . intercalate "\n"
    .(\lst -> map 
         (\targetLen -> map (\(_, len) -> if targetLen <= len then '*' else ' ') lst)
         $ reverse [1..maximum $ map snd lst])
    . map (\(x:xs) -> (x, length xs))
    . group 
    . sort
    . (++ [0..9])