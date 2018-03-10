<<<<<<< HEAD
module Test.Testable

import My.Testable

assertEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertEq g e = if g == e
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

assertNotEq : Eq a => (given : a) -> (expected : a) -> IO ()
assertNotEq g e = if not (g == e)
    then putStrLn "Test Passed"
    else putStrLn "Test Failed"

testAddIsSumming : IO ()
testAddIsSumming = assertEq (add 3 7) 10
=======
import Specdris.Spec
import Data.Vect 

addTwo : Nat -> Nat
addTwo x = x + 2

addZeros : Num e => Vect n e -> Vect (addTwo n) e
addZeros (x :: xs) = x :: addZeros xs
addZeros Nil = [0, 0]

myzip : Vect n a -> Vect n b -> Vect n (a,b)
myzip [] [] = []
myzip (x :: xs) (y :: ys) = (x, y)::(myzip xs ys)

-- twice : (n : Nat) -> (k ** k = n + n)
-- twice n = (n ** [k, n+n])

typedMin : (a: Nat) -> (b: Nat) -> (c ** c = (a + b) )
typedMin a b = ?sdklfj


-- johanzip : Vect n a -> Vect m b -> Vect (min n m) (a,b)
-- johanzip [] [] = []
-- johanzip [] (x :: xs) = []
-- johanzip (x :: xs) [] = []
-- johanzip (x :: xs) (y :: ys) = (x, y)::(johanzip xs ys)


pp : Vect n a -> Vect m a -> Vect (n + m) a
pp Nil       ys = ys
pp (x :: xs) ys = x :: ( pp xs ys)

main : IO ()
main = spec $ do
  describe "This is my math test" $ do
    it "adds two natural numbers" $ do
      (1 + 1) `shouldBe` 2
    it "multiplies two natural numbers" $ do
      (2 * 2) `shouldBe` 4
>>>>>>> TDDLyon
