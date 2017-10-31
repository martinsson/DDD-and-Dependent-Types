import Specdris.Spec

fizzbuzz : (a: Nat) -> String
fizzbuzz (S (S (S Z)) = "Fizz"
fizzbuzz a = show a

main : IO ()
main = spec $ do
  describe "fizzbuzz" $ do
    it "passes all rest" $ do
      map fizzbuzz [1, 2, 4, 7, 8, 11, 13] `shouldBe` ["1", "2", "4", "7", "8", "11", "13"]
    it "fizzez for 3's " $ do 
      fizzbuzz 3 `shouldBe` "Fizz"
