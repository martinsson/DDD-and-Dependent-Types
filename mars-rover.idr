import Specdris.Spec
import Data.SortedMap
import Data.Matrix.Numeric
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
%default total

Coord: Type
Coord = Vect 2 Integer -- perhaps this could be Fin of the board size

IntDirection : Type
IntDirection = Vect 2 Integer

data RoverState = MkPos Coord IntDirection 
  
RoverAction: Type
RoverAction = RoverState -> RoverState

Show RoverState where
  show (MkPos [x, y] dir) = show x ++ " " ++ show y ++ " " ++ showDirection dir where 
    showDirection: IntDirection -> String
    showDirection x = show (fromMaybe '$' (lookup x directions)) where
        directions : SortedMap IntDirection Char
        directions = fromList [
          ([ 0,  1], 'N'),
          ([-1,  0], 'W'),
          ([ 0, -1], 'S'),
          ([ 1,  0], 'E')]

-- why can't there be some default implem?
Eq RoverState where
  (==) (MkPos coord1 dir1) (MkPos coord2 dir2) = coord1 == coord2 && dir1 == dir2

turnLeft : RoverAction
turnLeft (MkPos coord direction) = let newDir = direction <\> leftRotation in
                                        MkPos coord newDir where
  leftRotation : Matrix 2 2 Integer
  leftRotation = [[0, 1], [-1, 0]] 

turnRight : RoverAction
turnRight state = turnLeft $ turnLeft $ turnLeft state

moveForward : RoverAction
moveForward (MkPos coord dir) = MkPos (coord + dir) dir

identity : RoverAction
identity state = state

getRoverAction: Char -> RoverAction
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


N : IntDirection
N = [0, 1]

W : IntDirection
W = [-1, 0]

S : IntDirection
S = [0, -1]

E : IntDirection
E = [1, 0]

main: IO ()
main = spec $ do 
  describe "rovers" $ do
    it "does something " $ do
      1 `shouldBe` 1
    it "simple turn" $ do
      moveRover (MkPos [1, 2] N) "L" `shouldBe` (MkPos [1, 2] W)
    it "simple turn not equal" $ do
      moveRover (MkPos [1, 2] N) "L" `shouldNotBe` (MkPos [1, 2] N)
    it "acceptance test" $ do
      moveRover (MkPos [1, 2] N) "LMLMLMLMM" `shouldBe` (MkPos [1, 3] N)
      moveRover (MkPos [3, 3] E) "MMRMMRMRRM" `shouldBe` (MkPos [5, 1] E)
  -- describe "position equality" $ do
    -- it "should not be equal" $ do
      -- N `shouldBe` W
