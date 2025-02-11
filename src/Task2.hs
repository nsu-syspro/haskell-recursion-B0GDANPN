{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- The above pragma enables all warnings
-- (except for unused imports from Task1)

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)

-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden
import Task1 (reverse, map, sum,doubleEveryOther, toDigits)

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN n toInt xs = (n - (s `mod` n)) `mod` n
  where
    nums = map toInt xs
    doubled = doubleEveryOther (reverse nums)
    normalized = map (\x -> if x >= n then x - (n - 1) else x) doubled
    s = sum normalized

-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise = error "Invalid hexadecimal digit"
-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec n = luhnDec (toDigits (n `div` 10)) == fromInteger (n `mod` 10)

-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

validateHex :: [Char] -> Bool
validateHex s = luhnHex (init s) == digitToInt (last s)
init :: [a] -> [a]
init [] = []
init [_] = []
init (x : xs) = x : init xs

last :: [a] -> a
last [] = error "Invalid empty list"
last [x] = x
last (_ : xs) = last xs