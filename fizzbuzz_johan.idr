%default total

IsFizz : (k : Nat) -> Type
IsFizz k = (modNatNZ k 3 SIsNotZ = 0)

IsBuzz : Nat -> Type
IsBuzz k = (modNatNZ k 5 SIsNotZ = 0)

data Fizzbuzz : (k: Nat) -> Type where
  Fizz : (k: Nat) -> {auto prf: IsFizz k} -> Not (IsBuzz k) -> Fizzbuzz k
  Buzz : (k: Nat) -> Not (IsFizz k) -> IsBuzz k -> Fizzbuzz k
  FizzBuzz : (k: Nat) -> IsFizz k -> IsBuzz k -> Fizzbuzz k
  Normal : (k: Nat) -> Not (IsFizz k) -> Not (IsBuzz k) -> Fizzbuzz k

fizzbuzz : (k: Nat) -> Fizzbuzz k
fizzbuzz k = let isFizz = decEq (modNatNZ k 3 SIsNotZ) 0 
                 isBuzz = decEq (modNatNZ k 5 SIsNotZ) 0 in 
                 case (isFizz, isBuzz) of
                       (Yes isfizz, No notbuzz) => Fizz k notbuzz
                       (No notfizz, Yes isbuzz) => Buzz k notfizz isbuzz
                       (Yes isfizz, Yes isbuzz) => FizzBuzz k isfizz isbuzz
                       (No notfizz, No notbuzz) => Normal k notfizz notbuzz

showFizzBuzz : Nat -> String
showFizzBuzz k = case fizzbuzz k of
                  Fizz k _     => "fizz"
                  Buzz k _ _     => "buzz"
                  FizzBuzz k _ _ => "fizzbuzz"
                  Normal k _ _   => show k
