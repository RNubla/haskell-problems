{- ##################################
 Robert Nubla
 Homework 4.
 ################################## -}
module Prog4 where
import Data.Char
import Data.List

doubleAll :: [Int] -> [(Int, Int)]
-- doubleAll _ = error "undefined"
doubleAll [] = []
doubleAll (x:xs) = (x, x*2) : doubleAll xs 

productLastPart :: Int -> [Int] -> Int
-- productLastPart _ = error "undefined"
productLastPart n xs
    |n == 0 = 1
    |n > 0 = last xs * productLastPart (n-1) (init xs)


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
-- iSort' [] = []
-- iSort' (x:xs) = inse

-- isUpper :: Char -> Bool
-- isUpper c = 

lowerFirstCharacter :: String -> String
-- lowerFirstCharacter _ = error "undefined"
lowerFirstCharacter (x:xs) = if isUpper(x) == True
                          then toLower (x) : xs
                          else x : lowerFirstCharacter xs

findSpaceIndex :: String -> [Int]
findSpaceIndex str = findIndices(==' ') str

middleWord :: String -> String
middleWord _ = error "undefined"
-- middleWord (x:xs) = if (x `elem` ' ':[]) == True
--                     then xs
--                     else x : middleWord xs

lowerFirstLetter :: String -> String
lowerFirstLetter (x:xs) = if isUpper(x) == True
                          then toLower(x) : xs
                          else x : lowerFirstLetter xs

lowerFirstTwoLetters :: String -> String
-- lowerFirstTwoLetters _ = error "undefined"
lowerFirstTwoLetters (x1:x2:xs) = if isUpper(x1) && isUpper(x2) == True
                                  then toLower(x1) : toLower (x2) : xs
                                  else x1 : x2: lowerFirstTwoLetters xs