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
parseInput : String -> Either String Pos

parseAvailablePos : (freePos: Vect n Pos) -> String -> Either String (n: Pos ** Elem n freePos) 
parseAvailablePos freePos x = ?parseAvailablePos_rhs

availablePos : Vect 2 Pos
availablePos = [(Upper, Left), (Upper, Center)] 


main : IO() 
main = do
  putStrLn "where do you want to put your mark?"
  x <- getLine
  case parseAvailablePos availablePos x of
       (Left l) => ?lsjf_1
       (Right r) => ?lsjf_2













