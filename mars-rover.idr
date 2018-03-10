import Specdris.Spec

-- Input
-- 5 5          grid size
-- 1 2 N        initial position and facing North
-- LMLMLMLMM    Left Move Left Move ...
-- 3 3 E        Another rover initial pos and facing
-- MMRMMRMRRM

-- Expected Output:
-- 
-- 1 3 N        Rover 1s final position
-- 5 1 E        Rover 2s ...

Coord: Type
Coord = (Nat, Nat) -- perhaps this could be Fin of the board size

data Direction: Integer -> Integer -> Type where
  S: Direction 0    (-1)
  W: Direction (-1) 0
  N: Direction 0    1
  E: Direction 1    0
               
Eq (Direction x y) where
  (==) d1 d2 = sameDir d1 d2 where
    sameDir: Direction x1 y1 -> Direction x2 y2 -> Bool
    sameDir _ _ {x1} {x2} {y1} {y2} = x1 == x2 && y1 == y2

Show (Direction x y) where
  show S = "S"
  show W = "W"
  show N = "N"
  show E = "E"
  
data Position = MkPos Coord (Direction x y)
  
Show Position where
  show (MkPos (a, b) w) = show a ++ " " ++ show b ++ " " ++ show w 

Eq Position where
  (==) (MkPos w s) (MkPos z t) = w == z -- && s == t

moveRover: Position -> (instructions: String) -> Position
moveRover x instructions = MkPos (1, 2) W

main: IO ()
main = spec $ do 
  describe "rovers" $ do
    it "simple turn" $ do
      moveRover (MkPos (1, 2) N) "L" `shouldBe` (MkPos (1, 2) W)
    it "simple turn not equal" $ do
      moveRover (MkPos (1, 2) N) "L" `shouldNotBe` (MkPos (1, 2) N)
  describe "position equality" $ do
    it "should work" $ do
      N `shouldBe` N
    it "should not be equal" $ do
      N `shouldBe` W
