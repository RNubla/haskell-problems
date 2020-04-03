 
{- ################################## 
    Robert Nubla.
    Homework 2.
################################## -}
module Prog2 where

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z
    |x == y || y == z || x == z = False
    |otherwise = True


fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent w x y z
    |w == x || w == y || w == z || x == y || y == z || x == z = False
    | otherwise = True

sum' :: Integer -> Integer
sum' 0 = 0
sum' n = n + sum'(n-1)

asciisum :: String -> Integer
-- asciisum _ = error "undefined"
asciisum n = fromIntegral (sum [(fromEnum str)| str <- n])


-- helper function
fromIntToDouble :: Integer -> Double
fromIntToDouble n = fromIntegral n

integerSqrt :: Integer -> Integer
integerSqrt n = (round(sqrt (fromIntToDouble n)))

-- helper function
maxOfThree :: Integer -> Integer -> Integer -> Integer
maxOfThree x y z = (max (max x y)) z

minOfThree :: Integer -> Integer -> Integer -> Integer
minOfThree x y z = (min (min x y)) z

midOfThree :: Integer -> Integer -> Integer -> Integer
midOfThree x y z
    |x > minOfThree x y z && x < maxOfThree x y z = x
    |y > minOfThree x y z && y < maxOfThree x y z = y
    |z > minOfThree x y z && z < maxOfThree x y z = z

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = ((minOfThree x y z), (midOfThree x y z), (maxOfThree x y z))

-- First and last swaps and middle two stays
swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
swap (w, x, y, z) = (z, x, y, w)

digitCount :: Integer -> Integer
digitCount n = toInteger (floor (logBase 10 (fromIntegral n)) + 1)

negateTwoDigits :: [Integer] -> [Integer]
negateTwoDigits num = [if (digitCount n == 2) then (negate n) else n | n <- num]

matches :: Integer -> [Integer] -> [Integer]
-- matches _ = error "undefined"
matches i iArr = [arr| arr <- iArr, arr == i]

element :: Integer -> [Integer] -> Bool 
-- element _ = error "undefined"
element i iArr = if (null (matches i iArr))
                    then
                        False
                    else
                        True 