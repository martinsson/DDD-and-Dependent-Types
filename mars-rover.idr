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
Coord = (Integer, Integer) -- perhaps this could be Fin of the board size

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

data RoverState = MkPos Coord IntDirection 
  
Show RoverState where
  show (MkPos (x, y) dir) = show x ++ " " ++ show y ++ " " ++ show dir 

-- why can't there be some default implem?
Eq RoverState where
  (==) (MkPos w s) (MkPos z t) = w == z && s == t

data Command = L | R | M | NoCommand

-- I could return the function instead of a type here
getCommand: Char -> Command
getCommand x = if x == 'L' 
                  then L
               else if x == 'R'
                  then R
               else if x == 'M'
                 then M 
               else NoCommand 

turnLeft : (state : RoverState) -> RoverState
turnLeft (MkPos coord (dx, dy)) = let leftComplexNumber = (0, 1) 
                                      (leftx, lefty) = leftComplexNumber  
                                      newDir = (dx * leftx - dy * lefty, dy * leftx + dx * lefty) in
                                        MkPos coord newDir

turnRight : (state : RoverState) -> RoverState
turnRight state = turnLeft $ turnLeft $ turnLeft state

moveForward : (state: RoverState) -> RoverState
moveForward (MkPos (x, y) dir@(dx, dy)) = MkPos (x + dx, y + dy) dir

moveRover: RoverState -> (instructions: String) -> RoverState
moveRover state instructions = moveRoverHelper state (unpack instructions) where
  moveRoverHelper: RoverState -> (commands: List Char) -> RoverState
  moveRoverHelper state [] = state
  moveRoverHelper state (c :: cs) = case getCommand c of 
                                         -- clearly here we need to get the function to apply instead
                                         L => moveRoverHelper (turnLeft state) cs
                                         R => moveRoverHelper (turnRight state) cs
                                         M => moveRoverHelper (moveForward state) cs
                                         NoCommand => moveRoverHelper state cs



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
      moveRover (MkPos (3, 3) E) "MMRMMRMRRM" `shouldBe` (MkPos (5, 1) E)
  -- describe "position equality" $ do
    -- it "should not be equal" $ do
      -- N `shouldBe` W
