import Specdris.Spec
import Data.SortedMap

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

IntDirection : Type
IntDirection = (Integer, Integer) 

N : IntDirection
N = (0, 1)

W : IntDirection
W = (-1, 0)

S : IntDirection
S = (0, -1)

E : IntDirection
E = (1, 0)

directions : List (IntDirection, String) 
directions = [(S, "S"),
              (W,  "W"),
              (N,  "N"),
              (E,  "E")]

data RoverState = MkPos Coord IntDirection 
  
Show RoverState where
  show (MkPos (x, y) dir) = show x ++ " " ++ show y ++ " " ++ show dir 

Eq RoverState where
  (==) (MkPos w s) (MkPos z t) = w == z && s == t

moveRover: RoverState -> (instructions: String) -> RoverState
moveRover x instructions = MkPos (1, 2) W

main: IO ()
main = spec $ do 
  describe "rovers" $ do
    it "does something " $ do
      1 `shouldBe` 1
    it "simple turn" $ do
      moveRover (MkPos (1, 2) N) "L" `shouldBe` (MkPos (1, 2) W)
    it "simple turn not equal" $ do
      moveRover (MkPos (1, 2) N) "L" `shouldNotBe` (MkPos (1, 2) N)
    it "acceptance test" $ do
      moveRover (MkPos (1, 2) N) "LMLMLMLMM" `shouldBe` (MkPos (1, 3) N)
  -- describe "position equality" $ do
    -- it "should not be equal" $ do
      -- N `shouldBe` W
