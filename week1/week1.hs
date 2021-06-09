import Data.Char

toDigits :: Integer -> [Integer]
toDigits cardId 
    | cardId <= 0 = []
    | otherwise = map (toInteger . digitToInt) $ show cardId

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs) 
    | length xs `mod` 2 == 1 = (2*x) : doubleEveryOther xs 
    | otherwise = x : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool  
validate =
    (== 0)
    . (`mod` 10) 
    . sumDigits 
    . doubleEveryOther 
    . toDigits