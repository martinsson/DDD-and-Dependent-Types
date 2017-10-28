import Specdris.Spec
import Data.Vect 

addZeros : Vect n e -> Vect (S (S (n))) e
addZeros Nil = the (Vect 2 e ) [0, 0]
addZeros (x :: xs) = x :: addZeros xs

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
