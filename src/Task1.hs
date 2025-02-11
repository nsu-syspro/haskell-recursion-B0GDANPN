{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)
-----------------------------------
--
-- Checks whether the last digit is a valid check digit
-- for the rest of the given number using Luhn algorithm
--
-- Usage example:
--
-- >>> validate 3456
-- False
-- >>> validate 34561
-- True
-- >>> validate 34562
-- False

validate :: Integer -> Bool
validate n = luhn (toDigits (n `div` 10)) == fromInteger (n `mod` 10)
-----------------------------------
--
-- Computes check digit for given digits using Luhn algorithm
--
-- Usage example:
--
-- >>> luhn [3,4,5,6]
-- 1
luhn :: [Int] -> Int
luhn n = innerFunc (sum (map normalize (doubleEveryOther (reverse n))))
  where
    innerFunc s = (10 - (s `mod` 10)) `mod` 10
-----------------------------------
--
-- Produces list of digits for given positive number;
-- otherwise (for zero and negative numbers) returns empty list
--
-- Usage example:
--
-- >>> toDigits 3456
-- [3,4,5,6]
-- >>> toDigits 0
-- []
-- >>> toDigits (-123)
-- []

toDigits :: Integer -> [Int]
toDigits 0 = []
toDigits n 
  | n < 0 = []
  | otherwise = toDigits (n `div` 10) ++ [fromInteger (n `mod` 10)]
-----------------------------------
--
-- Produces list in reverse order to the given one
--
-- Usage example:
--
-- >>> reverse "Hello"
-- "olleH"
-- >>> reverse [3,4,5,6]
-- [6,5,4,3]

reverse :: [a] -> [a]
reverse []       = []
reverse (n : xs) =  reverse xs ++ [n]
-----------------------------------
--
-- Doubles every other digit starting from first one
--
-- Usage example:
--
-- >>> doubleEveryOther [6,5,4,3]
-- [12,5,8,3]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [n] = [2 * n]
doubleEveryOther (n1:n2:xs) = 2 * n1 : n2 :doubleEveryOther xs

-----------------------------------
--
-- Normalizes given number to single digit by subtracting 9
-- if it is greater than or equal to 10
--
-- (Assumes inputs between 0 and 18)
--
-- Usage example:
--
-- >>> normalize 12
-- 3
-- >>> normalize 1
-- 1

normalize :: Int -> Int
normalize n
  | n>9 = n - 9
  | otherwise = n


-----------------------------------
--
-- Produces list with given function applied to each element
-- in given list
--
-- Usage example:
--
-- >>> map (\n -> n * 2) [1,2,3,4]
-- [2,4,6,8]
--
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (n:xs) = f n : map f xs


-----------------------------------
--
-- Computes sum of given list of numbers
--
-- Usage example:
--
-- >>> sum [3,8,5,3]
-- 19
-- >>> sum []
-- 0
sum :: [Int] -> Int
sum []     = 0
sum (n:xs) = n + sum xs
