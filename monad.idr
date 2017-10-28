import Specdris.Spec
import Prelude.Maybe


record Person where
    constructor MkPerson
    firstName, lastName : String
    age : Int

johan : Person
johan = MkPerson "Johan" "Martinsson" 30


getFromList : String -> Maybe Person
getFromList "Johan" = Just johan
getFromList _ = Nothing

printname : Maybe Person -> String
printname _ = ""

main : IO ()
main = spec $ do
  describe "This is my math test" $ do
    it "adds two natural numbers" $ do
      (printname $ getFromList "Johan") `shouldBe` "Johan Martinsson"



