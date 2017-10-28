module vadd
import Data.Vect

vadd : Num a => Vect n a -> Vect n a -> Vect n a
vadd [] [] = ?vadd_rhs_3
vadd (x :: xs) ys = ?vadd_rhs_2
