import Data.Vect 

%default total 

concat : List a -> List b -> List (a,b)
concat (x :: xs) (y :: ys) = (x, y) :: concat xs ys 
concat _ _ = []

concatVect : Vect n t1 -> Vect n t2 -> Vect n (t1, t2)
concatVect [] [] = []
concatVect (x :: xs) (y :: ys) = (x, y) :: concatVect xs ys




data Row = Upper | Middle | Bottom
parseRow : String -> Maybe Row
parseRow "U" = Just Upper
parseRow "M" = Just Upper
parseRow "B" = Just Upper
parseRow _   = Nothing

data Column = Left | Center | Right
parseCol : String -> Maybe Column
parseCol "L" = Just Left
parseCol "C" = Just Center
parseCol "R" = Just Right
parseCol _   = Nothing

Pos : Type 
Pos = (Row, Column)

parsePos : String -> Maybe Pos

placeMark : (freePos: Vect (S n) Pos) -> Pos-> Vect n Pos

parseInput : String -> String -> Either String Pos
parseInput r c = case (parseRow r, parseCol c) of
                      (Just a, Just b) => Either.Right (a, b)
                      (_, _) => Either.Left "input cannot be parsed to a row and column"


-- parseAvailablePos : (freePos: Vect n Pos) -> String -> Either String (n: Pos ** Elem n freePos) 
-- parseAvailablePos freePos x = ?parseAvailablePos_rhs
-- 
-- availablePos : Vect 2 Pos
-- availablePos = [(Upper, Left), (Upper, Center)] 

simpleMain : IO() 
simpleMain = do
  putStrLn "where do you want to put your mark? 
                  '(U)pper|(M)iddle|(B)ottom?"
  r <- getLine
  putStrLn "(L)eft|(C)enter|(R)ight'? "
  c <- getLine
  case parseInput r c of
       (Left l) => ?parseFailure
       (Right r) => ?parseSuccess

main : IO()
main = simpleMain











