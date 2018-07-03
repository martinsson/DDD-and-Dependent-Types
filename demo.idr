import Data.Vect 
%default total

partial
zip : List a -> List b -> List (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = []

zipVect : Vect n a -> Vect n b -> Vect n (a, b)
zipVect [] [] = []
zipVect (x :: xs) (y :: ys) = (x, y) :: zipVect xs ys

































-- concat : List a -> List b -> List (a,b)
-- concat (x :: xs) (y :: ys) = (x, y) :: concat xs ys 
-- concat _ _ = []
-- 
-- concatVect : Vect n t1 -> Vect n t2 -> Vect n (t1, t2)
-- concatVect [] [] = []
-- concatVect (x :: xs) (y :: ys) = (x, y) :: concatVect xs ys


