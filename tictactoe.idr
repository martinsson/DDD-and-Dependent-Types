import Data.Vect
%default total

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

bottomNotUpper : (Bottom = Upper) -> Void
bottomNotUpper Refl impossible

DecEq Row where
  decEq Upper Upper = Yes Refl
  decEq Middle Middle = Yes Refl
  decEq Bottom Bottom = Yes Refl
  decEq Bottom Upper = No bottomNotUpper
  decEq Upper Bottom = No (negEqSym bottomNotUpper)
  decEq a b = ?slkjdf

centerNotLeft : (Center = Left) -> Void
centerNotLeft Refl impossible

DecEq Column where
  decEq Left Left = Yes Refl
  decEq Center Left = No centerNotLeft
  decEq Left Center = No (negEqSym centerNotLeft)
  decEq Right x2 = ?DecEq_rhs_4
  decEq x1 x2 = ?DecEq_rhs_4

RowAndColInput : Type
RowAndColInput = (String, String)

placeMark : (freePos: Vect (S n) Pos) -> (p: Pos) -> (Elem p freePos) -> Vect n Pos
placeMark freePos _ elemProof = dropElem freePos elemProof

placeMarkClassic : (freePos: List Pos) -> Pos-> List Pos

parseInput : RowAndColInput -> Either String Pos
parseInput (r, c) = case (parseRow r, parseCol c) of
                      (Just a, Just b) => Either.Right (a, b)
                      (_, _)           => Either.Left "input cannot be parsed to a row and column"

parseAvailablePos : (freePos: Vect n Pos) -> RowAndColInput -> Either String (p ** Elem p freePos) 
parseAvailablePos freePos (r, c) = 
  case (parseRow r, parseCol c) of
    (Just row, Just col) => let validPos = (row, col) in  
                                (case isElem validPos freePos of 
                                      (Yes posIsFree) => Either.Right (validPos ** posIsFree)
                                      (No contra) => Either.Left "position is not free")
    (_, _)               => Either.Left "input cannot be parsed to a row and column"

availablePos : Vect 2 Pos
availablePos = [(Upper, Left), (Bottom, Left)] 

partial
simpleMain : IO() 
simpleMain = do
  putStrLn "where do you want to put your mark? "
  putStrLn "'(U)pper|(M)iddle|(B)ottom?"
  r <- getLine
  putStrLn "(L)eft|(C)enter|(R)ight'? "
  c <- getLine
  case parseInput (r, c) of
       (Left error) => putStrLn error
       (Right success) => putStrLn $ "OK occupying position " ++ (show (r, c) ) 
  putStrLn ""
  simpleMain

partial
strictMain : IO() 
strictMain = do
  putStrLn "where do you want to put your mark? "
  putStrLn "'(U)pper|(M)iddle|(B)ottom?"
  r <- getLine
  putStrLn "(L)eft|(C)enter|(R)ight'? "
  c <- getLine
  case parseAvailablePos availablePos (r, c) of
       (Left error) => putStrLn error
       (Right success) => putStrLn $ "OK occupying position " ++ (show (r, c) ) 
  putStrLn ""
  strictMain

partial
main : IO()
main = strictMain

