import Specdris.Spec

fizzbuzz : (a: Nat) -> String
fizzbuzz a = show a

main : IO ()
main = spec $ do
  describe "fizzbuzz" $ do
    it "passes all rest" $ do
      map fizzbuzz [1, 2, 4, 7, 8, 11, 13] `shouldBe` ["1", "2", "4", "7", "8", "11", "13"]
