x :: Int
x = 3

y :: Integer
y = 4 + 4

z :: Double
z = 3.14

---define an addOn function
addOne :: Int -> Int
addOne n = n + 1

timesThree :: Int -> Int
timesThree n = n * 3

-- a function to test wether three integers are eqial
threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m == n) && (n==p)

-- a function to test whether 4 integers are equal
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = (a == b) && (b == c) && (c == d)

fourEqual' :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual' a b c d = (threeEqual a b c) && (c == d)

-- char to enum
fromEnum 'A'
-- Prints out 65
(toEnum 65) :: Char
-- printys out 'A'
:type 3
-- PRints 3 :: Num a => a
:type (3::Int)

sqrt 5.5
-- print 2.345....
sqrt (5.5::Float)
-- print 2.345... shorter than top one
sqrt (5.5::Double)
-- print 2.345.... same as the first one coz its a double