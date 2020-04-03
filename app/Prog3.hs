{- ##################################
 Robert Nubla
 Homework 3.
 ################################## -}
module Prog3 where
import Data.Char
import Data.List

doubleAll :: [Int] -> [(Int, Int)]
-- doubleAll _ = error "undefined"
doubleAll x = [(z,z*2) | z <- x]

productLastPart :: Int -> [Int] -> Int
-- productLastPart _ = error "undefined"
productLastPart n l = n * last l

init' :: [Int] -> [Int]
-- init' _ = error "undefined"
init' n = take (length n-1) n

nestedParens :: String -> Bool
-- nestedParens _ = error "undefined"
nestedParens paren
    |paren == [] = True
    |paren !! 0 == '(' && paren !!((length paren) -1) == ')' = nestedParens(tail(init paren))
    |otherwise = False

triads :: Int -> [(Int,Int,Int)]
-- triads _ = error "undefined"
-- 3,4,5
triads n = [ (x,y,z) | x <- [0..n], y <- [0..n], z <- [0..n], x^2 + y^2 == z^2 ]

pushRight :: String -> Int -> String
pushRight str n
    |n >= length str, n <= 40 = (replicate (n - length str) ' ') ++ str
    |otherwise = "Invalid"

lowerFirstCharacter :: String -> String
-- lowerFirstCharacter _ = error "undefined"
lowerFirstCharacter x
    |isUpper(x !! 0) = [toLower x'| x' <- x]

-- helper function to find index of the space character
findSpacesIndex :: String -> [Int]
findSpacesIndex str = findIndices(==' ') str

-- helper function that returns the index of the middleWord function
-- and calls the drop function to drop that specific index
-- which then returns a string
takeSecondSpace :: String -> String
takeSecondSpace str = (drop ((head(findSpacesIndex str)) + 1) str)


middleWord :: String -> String
middleWord str =  takeSecondSpace (take (last (findSpacesIndex str)) str)

findUpperCharIndex :: String -> [Int]
findUpperCharIndex str = findIndices isUpper str

-- replaceChar :: Char -> Char
-- replaceChar [] = []
-- replaceChar (x:xs)=
--     if x == findUpperCharIndex

lowerFirstLetter :: String -> String
-- lowerFirstLetter str 
-- lowerFirstCharacter str = 
-- lowerFirstLetter [] = []
-- lowerFirstLetter (x:xs) = lowerFirstLetter [ x' | x' <- xs, isUpper (xs !! (head(findUpperCharIndex xs)))] ++ [x]
lowerFirstLetter _ = error "undefined"
-- lowerFirstLetter str = toLower (str !! (head (findUpperCharIndex str)))
-- lowerFirstLetter str = [ if isUpper x then toLower (x !! (head(findUpperCharIndex x))) else x| x <- str, isUpper (str !! (head (findUpperCharIndex str)))]
    -- | isLower (str !! 0) = [ x| x <- str]
    -- |isUpper (str !! (head (findUpperCharIndex str))) = [ toLower x| x <- str, isUpper (head (findUpperCharIndex x))]
    -- |isUpper ("heLLo wOrlD" !! (head (findUpperCharIndex "heLLo wOrlD")))
    -- |isUpper (str !! (head (findUpperCharIndex str))) = [ toLower x| x <- str]
    -- |isUpper (str !! (head (findUpperCharIndex str))) = str


lowerFirstTwoLetters :: String -> String
lowerFirstTwoLetters _ = error "undefined"