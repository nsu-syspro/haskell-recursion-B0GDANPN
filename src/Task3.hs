{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- Note: the above pragma enables all warnings

module Task3 where

-----------------------
-- Helper type synonyms

type Peg = String
type Move = (Peg, Peg)

-----------------------

-- Usage examples
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 l r tmp = [(l, r)]
hanoi n l r tmp =  hanoi (n-1) l tmp r ++ [(l, r)] ++ hanoi (n-1) tmp r l
