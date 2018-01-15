-- %default total


testMod: Nat -> Nat
testMod k = modNatNZ k 3 SIsNotZ

data Fizzbuzz : (k: Nat) -> Type where
  Fizz : (k: Nat) -> {auto prf : modNat k 3 = 0} -> Fizzbuzz k
  Buzz : (k: Nat) -> {auto prf : mod k 5 = 0} -> Fizzbuzz k
  FizzBuzz : (k: Nat) -> 
             {auto prf : modNat k 3 = 0} -> 
             {auto prf : mod k 5 = 0} -> 
             Fizzbuzz k
  Normal : (k: Nat) -> 
           (prf : (modNat k 3 = 0) -> Void ) -> 
           (prf : (mod k 5 = 0) -> Void ) -> 
           Fizzbuzz k

fizzbuzz : (k: Nat) -> Fizzbuzz k
fizzbuzz k = let isFizz = decEq (modNat k 3) 0 
                 isBuzz = decEq (mod k 5) 0 in 
                 case (isFizz, isBuzz) of
                       (Yes isfizz, No notbuzz) => Fizz k
                       (No notfizz, Yes isfizz) => Buzz k
                       (Yes isfizz, Yes isbuzz) => FizzBuzz k
                       (No notfizz, No notbuzz) => Normal k notfizz notbuzz
