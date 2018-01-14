

data Fizzbuzz : (k: Nat) -> Type where
  Fizz : (k: Nat) -> {auto prf : mod k 3 = 0} -> Fizzbuzz k
  Normal : (k: Nat) -> (prf : (mod k 3 = 0) -> Void ) -> Fizzbuzz k

fizzbuzz : (k: Nat) -> Fizzbuzz k
fizzbuzz k = case decEq (mod k 3) 0 of
                  (Yes prf) => Fizz k 
                  (No contra) => Normal k contra 
