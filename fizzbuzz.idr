import Specdris.Spec

fizzbuzz : (a: Nat) -> String
fizzbuzz a = case mod a 3 of 
                Z => "Fizz"
                _ => show a 

main : IO ()
main = spec $ do
  describe "fizzbuzz" $ do
    it "passes all rest" $ do
      map fizzbuzz [1, 2, 4, 7, 8, 11, 13] `shouldBe` ["1", "2", "4", "7", "8", "11", "13"]
    it "fizzez for 3's " $ do 
      map fizzbuzz [3, 6, 9, 12] `shouldBe` ["Fizz", "Fizz", "Fizz", "Fizz"] 
