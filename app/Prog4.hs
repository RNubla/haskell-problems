{- ##################################
 Robert Nubla
 Homework 4.
 ################################## -}
module Prog4 where
import Data.Char

doubleAll :: [Int] -> [(Int, Int)]
-- doubleAll _ = error "undefined"
doubleAll [] = []
doubleAll (x:xs) = (x, x*2) : doubleAll xs 

productLastPart :: Int -> [Int] -> Int
productLastPart _ = error "undefined"
-- productLastPart n [] = 0
-- productLastPart n [] = drop n [] * productLastPart n []


init' :: [Int] -> [Int]
-- init' _ = error "undefined"
init' [] = []
init' [x] = tail[x]
init' (x:xs) = x : init' xs

odds [] = []
odds [x] = [x]
odds (x1:x2:xs) = x1 : odds xs

lowerOddLetters :: String -> String
-- lowerOddLetters _ = error "undefined"
lowerOddLetters "" = ""
lowerOddLetters [x] = [x]
lowerOddLetters (x1:x2:xs) = (toLower x1 ): x2 : lowerOddLetters xs

replicate' :: Int -> Char -> String
-- replicate' _ = error "undefined"
replicate' n ch
    |n <= 0 = []
    |otherwise = ch : replicate'(n-1) ch


iSort' :: [(Int, String)] -> [(Int, String)]
iSort' _ = error "undefined"

-- isUpper :: Char -> Bool
-- isUpper c = 

lowerFirstCharacter :: String -> String
lowerFirstCharacter _ = error "undefined"
-- lowerFirstCharacter s = head s : lowerFirstCharacter (tail s)
-- lowerFirstCharacter s = tail s : lowerFirstCharacter s

middleWord :: String -> String
middleWord _ = error "undefined"

lowerFirstLetter :: String -> String
-- lowerFirstLetter _ = error "undefined"
-- lowerFirstLetter "" = ""
lowerFirstLetter [x] = [toLower x]
lowerFirstLetter (x:xs) = toLower x: lowerFirstTwoLetters xs 

firstTwoLetters :: String -> String
firstTwoLetters str = str!!0 : str!!1 :[]

lowerFirstTwoLetters :: String -> String
-- lowerFirstTwoLetters _ = error "undefined"
lowerFirstTwoLetters "" = ""
lowerFirstTwoLetters [x] = [x]
lowerFirstTwoLetters (x1:x2:xs) = toLower x1 : toLower x2: lowerFirstTwoLetters xs 