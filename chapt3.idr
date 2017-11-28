import Data.Vect

insert : Ord a => (x : a) -> (xsSorted : Vect len a) -> Vect (S len) a
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: insert x xs

insSort : Ord a => Vect n a -> Vect n a
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

-- exercices

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = (S (my_length xs))



