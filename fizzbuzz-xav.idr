import Data.So
%default total

fizz : Nat -> Bool
fizz n = (modNatNZ n 3 SIsNotZ) == 0

buzz : Nat -> Bool
buzz n = (modNatNZ n 5 SIsNotZ) == 0

data FizzBuzz : (n: Nat) ->  Type where
  Fizz : {auto fizz : So (fizz n)} -> FizzBuzz n
  Buzz : {auto buzz : So (buzz n)} -> FizzBuzz n
  NotFizz : {auto nfizz : So (not (fizz n)) } -> FizzBuzz n
  NotBuzz : {auto nfizz : So (not (buzz n)) } -> FizzBuzz n

data FizzBuzzCombo = Combo (FizzBuzz n, FizzBuzz n)

Show (FizzBuzz n) where 
  show Fizz = "Fizz" 
  show Buzz = "Buzz"
  show _ = ""

fizzBuzz : (n: Nat) -> List (FizzBuzz n)
fizzBuzz n  = case (choose (fizz n),choose (buzz n)) of
             (Left _,Right _) => [Fizz, NotBuzz]
             (Right _,Left _) => [NotFizz, Buzz]
             (Left _,Left _) => [Fizz, Buzz]
             (Right _,Right _) => [NotFizz, NotBuzz]

showFizzBuzz : Nat -> String
showFizzBuzz n = case (fizzBuzz n) of
                      [] => show n
                      f => concatMap show f
