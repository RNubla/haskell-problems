{- ##################################
 Robert Nubla.
 Homework 1.
 ################################## -}
module Prog1 where

isPositive :: Float -> Bool
isPositive n
    | n >= 0 = True
    | otherwise = False

dividesEvenlyByFive :: Integer -> Bool
dividesEvenlyByFive n
    | n `mod` 5 == 0 = True
    |otherwise = False

middle :: Integer -> Integer -> Integer -> Integer
middle x y z
    | x > y = y
    | y > z = y
    |z > y = y

nor :: Bool -> Bool -> Bool
-- nor _ = error "undefined"
nor False x = not x
nor True _ = not True


triangleArea :: Integer -> Integer -> Float
-- triangleArea _ = error "undefined"
triangleArea b h = (((fromIntegral b) * (fromIntegral h)))/2

ceilingDecimal :: Float -> Float
-- ceilingDecimal _ = error "undefined"
ceilingDecimal x  = fromIntegral (ceiling x)

letterGrade :: Integer -> String
-- letterGrade _ = error "undefined"
letterGrade g
    |g >= 93 = "A"
    |g >= 90 = "A-"
    |g >= 87 = "B+"
    |g >= 83 = "B"
    |g >= 80 = "B-"
    |g >= 77 = "C+"
    |g >= 73 = "C"
    |g >= 70 = "C-"
    |g >= 67 = "D+"
    |g >= 63 = "D"
    |g >= 60 = "D-"
    |otherwise = "F"

averageThree :: Integer -> Integer -> Integer -> Float
-- averageThree _ = error "undefined"
averageThree x y z = ((fromIntegral x) + (fromIntegral y) + (fromIntegral z))/3

-- helper function
maxCount:: Int -> Int -> Int -> (Int, Int)

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
-- howManyAboveAverage _ = error "undefined"
howManyAboveAverage x y z
    |x > fromIntegral(ceiling (averageThree x y z)) && y > fromIntegral(ceiling (averageThree x y z)) = 2
    |x > fromIntegral(ceiling (averageThree x y z)) && z > fromIntegral(ceiling (averageThree x y z)) = 2
    |y > fromIntegral(ceiling (averageThree x y z)) && z > fromIntegral(ceiling (averageThree x y z)) = 2
    |x > fromIntegral(ceiling (averageThree x y z)) = 1
    |y > fromIntegral(ceiling (averageThree x y z)) = 1
    |z > fromIntegral(ceiling (averageThree x y z)) = 1
    |otherwise = 0

howManyWithinThreshold :: Integer -> Integer -> Integer -> Float -> Integer
howManyWithinThreshold _ = error "undefined"
-- howManyWithinThreshold x y z a
    -- |

