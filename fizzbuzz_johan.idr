%default total

IsFizz : (k : Nat) -> Type
IsFizz k = (modNatNZ k 3 SIsNotZ = 0)

IsBuzz : Nat -> Type
IsBuzz k = (modNatNZ k 5 SIsNotZ = 0)

data Fizzbuzz : (k: Nat) -> Type where
  Fizz : (k: Nat) -> IsFizz k -> Not (IsBuzz k) -> Fizzbuzz k
  Buzz : (k: Nat) -> Not (IsFizz k) -> IsBuzz k -> Fizzbuzz k
  FizzBuzz : (k: Nat) -> IsFizz k -> IsBuzz k -> Fizzbuzz k
  Normal : (k: Nat) -> Not (IsFizz k) -> Not (IsBuzz k) -> Fizzbuzz k

fizzbuzz : (k: Nat) -> Fizzbuzz k
fizzbuzz k = let isFizz = decEq (modNatNZ k 3 SIsNotZ) 0 
                 isBuzz = decEq (modNatNZ k 5 SIsNotZ) 0 in 
                 case (isFizz, isBuzz) of
                       (Yes isfizz, No notbuzz) => Fizz k isfizz notbuzz
                       (No notfizz, Yes isbuzz) => Buzz k notfizz isbuzz
                       (Yes isfizz, Yes isbuzz) => FizzBuzz k isfizz isbuzz
                       (No notfizz, No notbuzz) => Normal k notfizz notbuzz

showFizzBuzz : Nat -> String
showFizzBuzz k = case fizzbuzz k of
                  Fizz k _ _     => "fizz"
                  Buzz k _ _     => "buzz"
                  FizzBuzz k _ _ => "fizzbuzz"
                  Normal k _ _   => show k


-- Tests --

threeIsFizz : showFizzBuzz 3 = "fizz"
threeIsFizz = Refl

sixIsFizz : showFizzBuzz 6 = "fizz"
sixIsFizz = Refl

partial
multiplesOf3AreFizz : (n: Integer) -> showFizzBuzz (fromIntegerNat n)  = "fizz"
multiplesOf3AreFizz 3 = Refl
multiplesOf3AreFizz 6 = Refl
multiplesOf3AreFizz 9 = Refl
multiplesOf3AreFizz 12 = Refl
multiplesOf3AreFizz 18 = Refl

partial
multiplesOf5AreFizz : (n: Integer) -> showFizzBuzz (fromIntegerNat n)  = "buzz"
multiplesOf5AreFizz 5 = Refl
multiplesOf5AreFizz 10 = Refl
multiplesOf5AreFizz 20 = Refl
multiplesOf5AreFizz 25 = Refl

partial
multiplesOf3And5AreFizzBuzz : (n: Integer) -> showFizzBuzz (fromIntegerNat n)  = "fizzbuzz"
multiplesOf3And5AreFizzBuzz 15 = Refl
multiplesOf3And5AreFizzBuzz 30 = Refl

partial
nonMultiples : (n: Integer) -> showFizzBuzz (fromIntegerNat n) = (show n)
nonMultiples 1 = Refl
nonMultiples 2 = Refl
nonMultiples 37 = Refl

multiplesOf3AreFizz2 : (n: Nat) -> (prf: modNat n 5 = 1) -> showFizzBuzz (n*3) = "fizz"
multiplesOf3AreFizz2 Z prf = ?multiplesOf3AreFizz2_rhs_1
multiplesOf3AreFizz2 (S k) prf = ?multiplesOf3AreFizz2_rhs_2

