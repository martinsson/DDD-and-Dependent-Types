import Specdris.Spec

main : IO ()
main = spec $ do
  describe "This is my math test" $ do
    it "adds two natural numbers" $ do
      (1 + 1) `shouldBe` 2
    it "multiplies two natural numbers" $ do
      (2 * 2) `shouldBe` 3
    it "do fancy stuff with complex numbers" $ do
      pendingWith "do this later"
