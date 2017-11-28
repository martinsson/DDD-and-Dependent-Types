-- Think of binary numbers: sequences of 0's and 1's.
-- How many n-digit binary numbers are there that
-- don't have two adjacent 1 bits?
-- 
-- For example, for three-digit numbers, Five of the
-- possible eight combinations meet the criteria:
-- 
--    000, 001, 010, 011, 100, 101, 110, 111.
-- 
-- What is the number for sequences of length 4, 5, 10, n?
import Data.Vect
flatten : List (List a) -> List a
flatten [] = []
flatten (x :: xs) = x ++ flatten xs


diversion : (n: Nat) -> List (List Nat)
diversion Z = [[]]
-- diversion (S Z) = (0 :: []) :: (1 :: [])
diversion (S Z) = [ (x :: xs) | x <- [0, 1], xs <- [[]]] 
diversion (S (S k)) = [ (x :: xs) | x <- [0, 1], xs <- [[0], [1]]] 


