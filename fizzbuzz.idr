import Specdris.Spec

fizzbuzz : (a: Nat) -> String
fizzbuzz a = show a

main : IO ()
main = spec $ do
  describe "This is my math test" $ do
    it "adds two natural numbers" $ do
      fizzbuzz 1 `shouldBe` "1"
    it "passes all rest" $ do
      map fizzbuzz [1, 2] `shouldBe` ["1", "2"]
