import Specdris.Spec
import Data.SortedMap
-- idris mars-rover.idr -p specdris -p contrib

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

-- use some kind of a map, with reversible key-values
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

-- use a type alias for RoverActions?

-- improvement use complex numbers in contrib
turnLeft : (state : RoverState) -> RoverState
turnLeft (MkPos coord (dx, dy)) = let lef90DegreesComplexNumber = (0, 1) 
                                      (leftx, lefty) = lef90DegreesComplexNumber  
                                      newDir = (dx * leftx - dy * lefty, dy * leftx + dx * lefty) in
                                        MkPos coord newDir

turnRight : (state : RoverState) -> RoverState
turnRight state = turnLeft $ turnLeft $ turnLeft state

moveForward : (state: RoverState) -> RoverState
moveForward (MkPos (x, y) dir@(dx, dy)) = MkPos (x + dx, y + dy) dir

identity : RoverState -> RoverState
identity state = state

getRoverAction: Char -> (RoverState -> RoverState)
getRoverAction x = if x == 'L' 
                      then turnLeft
                   else if x == 'R'
                      then turnRight
                   else if x == 'M'
                     then moveForward 
                   else identity 


-- should we rather build the list of actions perhaps? then just fold?
moveRover: RoverState -> (instructions: String) -> RoverState
moveRover state instructions = moveRoverHelper state (unpack instructions) where
  moveRoverHelper: RoverState -> (commands: List Char) -> RoverState
  moveRoverHelper state [] = state
  moveRoverHelper state (c :: cs) = let action = getRoverAction c in 
                                        moveRoverHelper (action state) cs



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
