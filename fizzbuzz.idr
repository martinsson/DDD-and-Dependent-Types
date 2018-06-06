import Specdris.Spec

fizzbuzz : (a: Nat) -> String
fizzbuzz a = case [mod a 3, mod a 5] of 
                [Z, Z] => "FizzBuzz"
                [Z, _] => "Fizz"
                [_, Z] => "Buzz"
                _ => show a 


main : IO ()
main = spec $ do
  describe "fizzbuzz" $ do
    it "passes all rest" $ do
      map fizzbuzz [1, 2, 4, 7, 8, 11, 13] `shouldBe` ["1", "2", "4", "7", "8", "11", "13"]
    it "fizzez for 3's " $ do 
      map fizzbuzz [3, 6, 9, 12] `shouldBe` ["Fizz", "Fizz", "Fizz", "Fizz"] 
    it "buzzez for 5's " $ do
      map fizzbuzz [5] `shouldBe` ["Buzz"]
    it "fizzbuzzes for 15's " $ do
      map fizzbuzz [15] `shouldBe` ["FizzBuzz"]
