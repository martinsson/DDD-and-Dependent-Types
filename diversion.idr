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
diversion (S Z) = [[0], [1]]
diversion (S (S k)) = [ (x :: y :: xs) | 
                  x <- [Z , 1], 
                  (y :: xs) <- diversion (S k), 
                  not (x == 1 && y == 1)] 

-- oh so lenght diversion n is the fibonacci suite
