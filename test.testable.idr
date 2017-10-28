import Specdris.Spec
import Data.Vect 

addTwo : Nat -> Nat
addTwo x = x + 2

addZeros : Num e => Vect n e -> Vect (addTwo n) e
addZeros (x :: xs) = x :: addZeros xs
addZeros Nil = [0, 0]

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
