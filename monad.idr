import Specdris.Spec
import Prelude.Maybe


record Person where
    constructor MkPerson
    firstName, lastName : String
    age : Int

johan : Person
johan = MkPerson "Johan" "Martinsson" 30


getFromList : String -> Maybe Person
getFromList "Johan" = Just $ MkPerson "Johan" "Martinsson" 30
getFromList "Bobby" = Just $ MkPerson "Bobby" "Joe" 55
getFromList _ = Nothing

printname : Maybe Person -> String
printname Nothing = ""
printname (Just x) = firstName x ++ " " ++ lastName x

main : IO ()
main = spec $ do
  describe "This is my math test" $ do
    it "adds two natural numbers" $ do
      (printname $ getFromList "Johan") `shouldBe` "Johan Martinsson"
    it "empty" $ do
      (printname $ getFromList "Bobby") `shouldBe` "Bobby Joe"




