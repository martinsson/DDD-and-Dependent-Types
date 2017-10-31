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

fullname : Person -> String 
fullname x = firstName x ++ " " ++ lastName x

printname : Maybe Person -> String
printname Nothing = "No Person found"
printname (Just x) = fullname x

assertOld : Int -> Person -> Either () Person
assertOld x y = Right johan

printFor : String -> String
printFor = printname . getFromList


main : IO ()
main = spec $ do
  describe "This is my math test" $ do
    it "adds two natural numbers" $ do
      (printFor "Johan") `shouldBe` "Johan Martinsson"
    it "empty" $ do
      (printFor "Bobby") `shouldBe` "Bobby Joe"
    it "empty" $ do
      (printFor "No existing person") `shouldBe` "No Person found"





