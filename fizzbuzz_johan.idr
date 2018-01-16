%default total

modula5 : (dividend: Nat) -> Nat
modula5 Z = 0
modula5 (S Z) = 1
modula5 (S (S Z)) = 2
modula5 (S (S (S Z))) = 3
modula5 (S (S (S (S Z)))) = 4
modula5 (S (S (S (S (S k))))) = modula5 k

modula3 : (dividend: Nat) -> Nat
modula3 Z = 0
modula3 (S Z) = 1
modula3 (S (S Z)) = 2
modula3 (S (S (S j))) = modula3 j

data Fizzbuzz : (k: Nat) -> Type where
  Fizz : (k: Nat) -> {auto prf : modula3 k = 0} -> Fizzbuzz k
  Buzz : (k: Nat) -> {auto prf : modula5 k = 0} -> Fizzbuzz k
  FizzBuzz : (k: Nat) -> 
             {auto prf : modula3 k = 0} -> 
             {auto prf : modula5 k = 0} -> 
             Fizzbuzz k
  Normal : (k: Nat) -> 
           (prf : (modula3 k = 0) -> Void ) -> 
           (prf : (modula5 k = 0) -> Void ) -> 
           Fizzbuzz k

fizzbuzz : (k: Nat) -> Fizzbuzz k
fizzbuzz k = let isFizz = decEq (modula3 k) 0 
                 isBuzz = decEq (modula5 k) 0 in 
                 case (isFizz, isBuzz) of
                       (Yes isfizz, No notbuzz) => Fizz k
                       (No notfizz, Yes isfizz) => Buzz k
                       (Yes isfizz, Yes isbuzz) => FizzBuzz k
                       (No notfizz, No notbuzz) => Normal k notfizz notbuzz
