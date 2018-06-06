import Data.Vect 

%default total 

concat : List a -> List b -> List (a,b)
concat (x :: xs) (y :: ys) = (x, y) :: concat xs ys 
concat _ _ = []

concatVect : Vect n t1 -> Vect n t2 -> Vect n (t1, t2)
concatVect [] [] = []
concatVect (x :: xs) (y :: ys) = (x, y) :: concatVect xs ys




data Row = Upper | Middle | Bottom
data Column = Left | Center | Right


Pos : Type 
Pos = (Row, Column)

parsePos : String -> Maybe Pos

placeMark : (freePos: Vect (S n) Pos) -> Pos-> Vect n Pos

main : IO() 
main = do
  putStrLn "where do you want to put your mark?"
  x <- getLine
  case parsePos x of
       Nothing => ?lkjf_1
       (Just x) => ?lkjf_2













