max' :: Int -> Int -> Int
max3 :: Int -> Int -> Int -> Int
max4 :: Int -> Int -> Int -> Int -> Int

fac :: Int -> Int
sumFacs :: Int -> Int


fac n
-- first guard define base case 2nd is recursive steps fac(n-1) * n
    |  n == 0 = 1                    --base case
    |  n > 0 = fac (n-1) * n         --recursive
    | otherwise = error "fac not defined for negatives"

-- fn that sums the factorials 0! + 1! + ... + n!
sumFacs n
    | n == 0 = 1    --base case
    | n > 0 = sumFacs (n-1) + fac n


-- fn that given a natural numvers m and n
-- returns the product m * (m+1) *..*(n-1)*n
-- ex: rangeProduct 3 6
-- result: 3 * 4 * 5 * 6

rangeProduct :: Int -> Int -> Int
rangeProduct m n
    | m == n = m
    | m < n = m * rangeProduct (m+1) n

max' x y
    | x >= y = x
    | y > x  = y

max3 x y z = (x `max'` y) `max'` z

max4 w x y z = (max3 w x y) `max'` z

-- Class 5
-- implicit type conversion
-- double x = 3 -> 3.0
-- int y = (int) 3.5

-- explicit
-- fromIntegral :: Int -> Float
-- round    :: Float -> Int
-- floor    :: Float -> Int
-- ceiling  :: Float -> Int
percent :: Int -> Int -> Float
percent x y = (fromIntegral x)/(fromIntegral y) * 100

isEven :: Integer -> Bool
isEven n  = n `mod` 2 == 0

isEven' n
    | n `mod` 2 == 0 = True
    | otherwise     = False

-- Pattern matching
-- Use literals on the left hand side of a definition

not' :: Bool -> Bool
not' n 
    |n == True = False
    |n == False = True

not'' :: Bool -> Bool
not'' True = False
not'' False = True

f :: Int -> Int
f 0 = 10
f 1 = 11
f _ = 12

xor' :: Bool -> Bool -> Bool
xor' True x = not x
xor' False x = x

summation :: Int -> Int
summation 0 = 0 -- Base Case
summation n = n + summation(n-1)

-- && function
-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- True && False = False
-- False && True = False
-- False && False = False
--  _ && _ = False          _ = Wildcard pattern

-- Tuples & List
-- tuple: fixed # of values of fixed types ()
-- list : arbitrary # of values, all of the same type []
p :: (Int, Char)
p = (3 , 'x')
-- Type (t1, t2, ..., tn)
-- consists of values (v1, v2,..., vn)
--      where v1 :: t1, v2 :: t2,.. vn :: tn
x:: [Int]
x = [1,2,3,4]
-- [e1, e2,...en] has type [t]
-- where e1 :: t
--       e2 :: t
--       ......
--       en :: t
-- [1..100] prints 1 thu 100
-- [1,3..9] increaments by 2
-- decrement by [10,9..1]
-- ['a'..'k']
-- ['h''e''l''l''o'] == "hello" returns true
-- [3.1..7.0]
-- [0.0, 0.3 .. 1.0]
-- nested :: [[Int]]
-- nested = [[2], [3,5], [], [4,99]]
-- minAndMax :: Int -> Int ->(Int, Int)
-- >minAndMax 5 3
-- (3, 5)
-- minAndMax x y
--  |x < y = (x, y)
--  |otherwise = (y,x)

-- addPair :: (Int, Int) -> Int
-- addPair (3,4)

-- Selector Functions
-- fst(3,5) > 4 snd ('a'.'e') > 'e'
addPair :: (Int, Int) -> Int
-- addPair p = fst p + snd p
addPair (0, y) = y
addPair (x,y) = x + y

-- pattern matching
-- addPair (x,y) = x + y //prefered

-- addPair (0, y) = y
-- addPair (x,y) = x + y //will match everything
-- >addPair(0,17) >17

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
    |x < y = (x, y)
    |otherwise = (y,x)

-- shift :: ((Int, Int),Int) -> (Int, (Int, Int))
-- shift ((x,y),z) = (x, (y,z))
-- shift ((3,4), 5)
-- return (3, (4,5))
shift :: ((Int, Int),Int) -> (Int, (Int, Int))
shift ((x,y),z) = (x, (y,z))

maxOccurs :: Int -> Int -> (Int, Int)
-- >maxOccurs 3 4
-- >(4,1)
maxOccurs x y
    |x == y = (x, 2)
    |otherwise = (max x y, 1)

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z
    |z > fst (maxOccurs x y) = (z,1)
    |z == fst(maxOccurs x y) = (z, snd(maxOccurs x y) + 1)
    |otherwise               = maxOccurs x y

-- Right associative meaning parenthesis is optional
-- 3:[] or [3]
-- 7:(3:[]) = [7,3]
-- 9:7:3:[] = [9,7,3]


-- List comprehension
-- 1. a generator: a list that elements are drawn from
-- 2. optional: a test; use for filtering [boolean exp]; if True, keep some element
-- 3. transformation: some type of mapping to form the result set

-- {x | 2 < x < 20} || {x E Z}
-- [ x*x    | x <- [1,2,3]] -> [1,4,9]
--transformation       1. generator

-- ex = [2,4,7]
-- >[2 * n | n <- ex] -> [4,8,14]
-- [(x, even x) | x <- [1..4]]      even is built in

-- lowers :: String -> String
-- lowers s = [toLower ch | ch <-s ] -- >lowers "Hello" > "hello"

-- guard in list comprehension
-- addOrdPairs :: [(Int,Int)] - [Int]
-- addOrdPairs p = [x+y|(x,y)<- p, x <= y, x >=3 ] AND operator ,
-- addOrdPairs p = [x+y|(x,y)<- p, x <= y || x >=3 ] OR operator 
-- filter out 3 or above
-- [x|x<-[1,2,3,4], x <= 2]

allEven :: [Int] -> Bool
allEven nums = [n | n <- nums, even n] == nums
-- allEven nums = [n | n <- nums, odd n] == []

isPrime :: Int -> Bool
divisors :: Int -> [Int]     -- >divisors 12 > [1,2,3,4,12]

divisors n = [ m| m <- [1..n], n `mod` m == 0 ]
-- isPrime n = (snd (divisors n)) == n
isPrime n = divisors n == [1,n]

-- multiple generators, similar to inner and outer for loops in java
-- [(x,y) | x <- [1,2,3], y <- [1,2]]
-- > [(1,1), (1,2), (2,1), (2,2), (3,1), (3,2)]
multiGen :: [(Int, Int)]
multiGen = [(x,y) | x <- [1,2,3], y <- [1,2]]
--  [(x,y) | x <- [1,2,3], y <- [x..3]]
-- > [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

find :: Char -> [(Char, Int)] -> [Int]  -- Lookup table
-- find 'b' = [('a',1), ('b',2), ('c',3), ('b',4)]
-- >[2,4]
find k t = [ v |(k',v) <- t, k == k']

-- List Library Functions
-- Already built in haskell no import
-- > head [1,2,3,4,5] return 1
-- > head [] returns error
-- > tail [1,2,3,4,5] returns [2,3,4,5]
-- > [1,2,3,4,5] !! 2 returns 3 gets index
-- > take 3 [1,2,3,4,5] returns [1,2,3]
-- > drop 3 [1,2,3,4,5] returns [4,5]
-- length [2..5] returns 4
-- sum [1,2,3] returns 6
-- product [2..5] return 120
-- reverse [1,2,3,4,5] return [5,4,3,2,1]
-- 3:[4,5] returns [3,4,5] cons
-- append [2,3]++[4,5] returns [2,3,4,5]
-- and [True, False, True] returns True if every bool is true, but here its false
-- or [True, False,True] returns True
-- last [1,2,3,4,5] returns 5
-- init [1,2,3,4,5] returns [1,2,3,4] not the last NUM
-- replicate 3 'c' returns "ccc"
-- replicate 4 2 returns [2,2,2,2]
-- splitAt 3 [2,3,4,5,6] returns ([2,3,4], [5,6])
-- zip ['a','b','c'] [1,2,3,4] returns [('a',1),('b',2),('c',3)] // stops when elements are not even
-- unzip [('a',1), ('b',2), ('c',3)] returns ("abc",[1,2,3])

length' :: [Int] -> Int
length' xs = sum [1 | x <- xs]
-- > length' [3,4,5]
-- > sum[1,1,1]
-- > 3

howManyAreLowerCase :: String -> Int
-- > howManyAreLowerCase "Hello2" return 4
howManyAreLowerCase xs = length [ch|ch <- xs, ch >= 'a' && ch <='z']

-- write a function rangeSum that when given natural numbers m and n returns the sum
-- m+(m+1)+..+(n-1)+n m<= n
rangeSum :: Int -> Int -> Int
rangeSum m n = sum [m..n]

rangeSum' :: Int -> Int -> Int
rangeSum' m n
    |m == n = n
    |m <= n = m + rangeSum' (m+1) n
    |m > n = rangeSum' n m

-- Trace
-- rangeSum 3 6
-- 3 + rangeSum 4 6
-- 4 + rangeSum 5 6

countOnes :: String -> Int
countOnes s = sum[1|x <- s, x == '1']

-- allVowel :: String -> String
-- allVowel s [ch|ch <- s, ch=='a'||
--                         ch=='e'||
--                         ch=='i'||
--                         ch=='o'||
--                         ch=='u']

doubleAll :: [Int] -> [Int]
doubleAll xs = [2*x| x <- xs]

pairs :: [Int] -> [(Int, Int)] --pairs [1,2,3,4] -> [(1,2), (2,3), (3,4)]
pairs a = zip a (tail a)
--        zip[1,2,3,4][2,3,4]
--        zip[(1,2), (2,3), (3,4)]

sorted :: [Int] -> Bool -- sorted [1,3,7,8] -> true
sorted n = and[x <= y | (x,y) <- pairs n]

hypo :: Int -> Int -> Float
hypo a b = sqrt(fromIntegral ((a*a) + (b*b)))

-- threeEquals:: [Int] -> Bool
-- threeEquals i = [ | x <- i, x == x]

-- null [2,3,5] > False
-- null [] > true

-- pattern matching on list
-- [2,5,9] is the same as 2:5:8:[]
-- 1. a list matches [] when its empty
-- 2. a list mathces pattern (p:ps)
--      if its non-empty,
    --  and its head matches p,
    --  and its tail matches ps

-- [2,5,9]   (x:xs); 2----> x; [5,9] -----> xs;
-- [2,5,9]   (x:y:zs); 2----> x; 5 -----> y; [9] ----> zs
-- [2,5]  (x:y:zs);     2---->x; 5---->y; []----->zs

-- pattern that doesnt match
-- [2] its also 2:[]  (x:y:zs) 2 --->x, []--->y, zs has nothing, so it doesn match

-- list comprehension recursion
sum'' :: [Int] -> Int
sum'' [] = 0
sum'' (x:xs) = x + sum'' xs
-- sum''[2,5,1]
-- 2 + sum [5,1]
-- 2 + (5 + sum [1])
-- 2 + (5 + (1 + sum []))
-- 2 + 5 + 1 + 0

product'' :: [Int] -> Int
product'' (x:[]) = x
-- product'' [] = error
product'' [] = 1
product'' (x:xs) = x * product'' xs

length'' :: [Int] -> Int
length'' [] = 0
length'' (x:xs) = 1 + length'' xs -- use '_' for (_:xs) to symbolize that its not being used

reverse'' :: [Int] -> [Int]
reverse'' [] = []
reverse'' (x:xs) =  reverse'' xs ++ [x]
-- revers [1,5,9]
-- revers [5,9] ++ [1]
-- revers [9] ++ [5]
-- revers [] ++ [9]
-- []
-- results -> [9,5,1]

-- (+++) :: [Int] -> [Int] -> [Int]
(+++) :: [Int] -> [Int] -> [Int]
[] +++ ys = ys
(x:xs) +++ ys = x:(xs +++ ys)

-- [1,3] +++ [2,5]
-- 1 :([3] +++ [2,5])
-- 1: (3: ([] +++ [2,5]))
-- 1 : 3 : [2,5]

selectEven :: [Int] -> [Int]
selectEven xs = [  x |x <- xs, even x]

selectEven' :: [Int] -> [Int]
selectEven' [] = []
selectEven' (x:xs)
    |even x = x : selectEven' xs
    |otherwise = selectEven' xs

doubleAll' :: [Int] -> [Int]
-- [2,5]
-- [4,10]
-- list comprehension
-- doubleAll' xs = [ 2*x | x <- xs]
-- recursive
doubleAll' [] = []
doubleAll' (x:xs) = 2*x : doubleAll' xs

-- zip' :: [Int] -> [Int] -> [Int]
-- zip' [] [] = []
-- zip' [] ys = []
-- zip' xs [] = []
-- zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- zip less base case
-- zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
-- zip' _ _ = [] base case has to be second since using _ wildcard

-- unique :: [Int] -> [Int]
-- [2,3,3,2,1]
-- [2,3,1]
-- unique (x:xs) = 

-- InsertionSort
-- [5,9,3,4] take head:7 -> sort tail [3,4,9] insert 5 -> [3,4,5,9]
-- iSort :: [Int] -> [Int]
-- ins :: Int -> [Int] -> [Int]
            -- sorted tail
-- iSort [] = []
-- iSort (x:xs) = ins x ( iSort xs)

-- ins n [] = [n]
-- ins n (x:xs) 
    -- |n <= x = n : x : xs
    -- |n > x = x : ins n xs

-- > ins 5 [3,4,9]
-- > 3 : ins 5 [4,9]
-- >    4 : ins 5 [9]
--          5 : 9 : []
-- > [3,4,5,9]

-- QuickSort
-- [4,3,9,8,2,1] grab head: <= 4 >=
                        -- [3,2,1] [9,8]
                        -- [1,2,3] [8:9]

-- qSort [] = []
-- qSort (x:xs) =  qSort [ x'  | x' <- xs, x' <= x]   [3,2,1]
                            -- ++ [x] ++
                -- qSort [ x' | x' <- xs, x' >= x]   [9,8]
                -- returns [1,2,3] [8,9] -> [1,2,3,4,8,9]

-- mutual recursion
even' :: Int -> Bool
even' 0 = True
even' n = odd (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n-1)

-- even 3
-- odd 2
-- even 1
-- odd 0
-- False

-- evens "abcde"
-- "ace"
evens :: [Char] -> [Char]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [Char] -> [Char]
odds [] = []
odds (x:xs) = evens xs

triArea :: Float -> Float -> Float -> Float
triArea a b c =
            sqrt(s *(s-a) * (s-b) * (s-c))
            where 
                s = (a + b + c)/2
                 
-- rectArea :: Float -> Float -> Float can be ommitted
rectArea x y
    |possible = area
    |otherwise = 0
    where
        area     = x * y
        possible = x >= 0 && y >=0

maxsq :: Int -> Int -> Int
maxsq x y
    |sqx > sqy = sqx
    |otherwise = sqy
    where
        sqx = sq x
        sqy = sq y
        sq :: Int -> Int
        sq x = x * x

-- case expression
-- case  expression   of
    -- p1 -> e1
    -- p2 -> e2
    -- p3 -> e3
    -- pn -> en
-- ex01 :: Int
-- ex01 = case "Hello" of
--     []       -> 3
--     ('H':xs) -> length xs
--     _        -> 7

digits :: String -> String
digits st = [c| c <- st, c > '0' && c <='9']

-- firstDigit :: String -> Char
-- firstDigit st
--     |null (digits st) = '\0'
--     |otherwise = head (digits st)

firstDigit st = 
    case (digits st) of
        [] -> '\0'
        (x:xs) -> x

head' :: [Int] -> Int
head' [] = error "empty list"
head' (x:_) = x

-- head xs = 
    -- case xs of
        -- [] = error
        -- (x:_) = x

-- Type variables
-- head :: [a] -> a   a could be anything

-- type inference
-- mystery(x,y) if x then 'c' else 'd'
    -- mystery :: (Bool, a) -> Char type signiture

-- g x y z = (x `div` z) + 2
-- type sig
-- g :: Int -> a -> Int -> Int

-- Polymorphic functions vs Overloaded Functions
--  EX JAVA
-- foo (int x){}
-- foo (double x, int x){}

-- EQIALITY IS AN OVERLOADED FUNCTION IN HASKELL
-- Int == Int
-- 4 == 4
-- TUPLE
-- EX
-- (3,4) == (3,4)
-- (m,n) == (x,y) = m ==x && n == y

-- POLYMORPHIC
-- fst (x,y) = x
-- fst :: (a,b) -> a

-- Type synonyms
-- type String = [Char]

-- type ShopItem = (String, Int)
-- type Cart = [ShopItem]

-- totalCart :: Cart -> Int
        -- [same as]
        -- [Shopping Item]
        -- [(String,Int)]

-- totalCart [] = 0
-- totalCart ((item, price):xs) = price + totalCart xs

-- parameterizarion
-- type PairInt = (Int, Int)
-- type PairChar = (Char, Char)
-- type PairBool = (Bool, Bool)

-- -- THSE BECOMES 
-- type Pair a = (a,a)
-- i :: Pair Int
-- i = (3,4)

-- type Map k v = [(k,v)]
-- m1 :: Map Char Int
-- m2 :: Map Int Bool
-- m1 = [('a',1),('z',99)]
-- [(5,True),(7, False)]

-- find' :: Char -> [(Char, Int)] -> [Int]
-- Becomes more general
-- find' :: a -> [(a, b)] -> [b]
-- Then becomes 
-- find' :: k -> Map k v -> [v]
-- find' k m = [v | (k',v) <- m, k==k']

-- Algebraic Data Types
-- data Bool = True | False
-- data Student = Freshman
            --  | Soph
            --  | Junior
            --  | Senior
            --  | SuperSenior
-- joe :: Student
-- joe = Junior
-- shouldGraduate :: Student -> Bool
-- shouldGraduate Senior = True
-- shouldGraduate SuperSenior = True
-- shouldGraduate _ = False