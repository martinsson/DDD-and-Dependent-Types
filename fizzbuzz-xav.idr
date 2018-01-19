import Data.So
%default total

fizz : Nat -> Bool
fizz n = (modNatNZ n 3 SIsNotZ) == 0

buzz : Nat -> Bool
buzz n = (modNatNZ n 5 SIsNotZ) == 0

data FizzBuzz : (n: Nat) ->  Type where
  Fizz : {auto fizz : So (fizz n)} -> FizzBuzz n
  Buzz : {auto buzz : So (buzz n)} -> FizzBuzz n

Show (FizzBuzz n) where 
  show Fizz = "Fizz" 
  show Buzz = "Buzz"

fizzBuzz : (n: Nat) -> List (FizzBuzz n)
fizzBuzz n  = case (choose (fizz n),choose (buzz n)) of
             (Left _,Right _) => [Fizz]
             (Right _,Left _) => [Buzz]
             (Left _,Left _) => [Fizz, Buzz]
             (Right _,Right _) => []

showFizzBuzz : Nat -> String
showFizzBuzz n = case (fizzBuzz n) of
                      [] => show n
                      f => concatMap show f
