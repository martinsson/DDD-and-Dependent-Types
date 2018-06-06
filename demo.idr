import Data.Vect 
import Data.So

concat : List a -> List b -> List (a,b)
concat (x :: xs) (y :: ys) = (x, y) :: concat xs ys 
concat _ _ = []

concatVect : Vect n t1 -> Vect n t2 -> Vect n (t1, t2)
concatVect [] [] = []
concatVect (x :: xs) (y :: ys) = (x, y) :: concatVect xs ys


