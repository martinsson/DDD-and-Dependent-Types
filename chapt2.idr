import Data.Vect

palindrome : Nat -> String -> Bool
palindrome n s = case length s < n of 
                True => False
                False => reverse (toLower s) == (toLower s)

-- toptenVect : (n: Nat) => Vect (10 + n) Nat -> Vect 10 Nat
-- toptenVect xs = take 10 Vect.fromList (sort xs)
-- toptenVect xs = take 10 sort xs

topten : Ord a => List a -> List a
topten xs = List.take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length n xs = List.length (filter longEnough xs)
  where longEnough = \s => length s > n

main : IO () 
main = repl "Enter a possible palindrome: " ( \s => show (palindrome 1 s) ++ "\n")


