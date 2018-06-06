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


-- Tests --

threeIsFizz : showFizzBuzz 3 = "fizz"
threeIsFizz = Refl

sixIsFizz : showFizzBuzz 6 = "fizz"
sixIsFizz = Refl

multiplesOf3AreFizz : (n: Nat) -> showFizzBuzz (n*3) = "fizz"
multiplesOf3AreFizz Z = ?multiplesOf3AreFizz_rhs_1
multiplesOf3AreFizz (S Z) = Refl
multiplesOf3AreFizz (S (S Z)) = Refl
multiplesOf3AreFizz (S (S (S Z))) = Refl
multiplesOf3AreFizz (S (S (S (S k)))) = ?multiplesOf3AreFizz_rhs_5


multiplesOf3AreFizz2 : (n: Nat) -> (prf: modNat n 5 = 1) -> showFizzBuzz (n*3) = "fizz"
multiplesOf3AreFizz2 Z prf = ?multiplesOf3AreFizz2_rhs_1
multiplesOf3AreFizz2 (S k) prf = ?multiplesOf3AreFizz2_rhs_2


multiplesOf3AreFizz3 : (n ** modNat n 5 = 1 ) -> showFizzBuzz (n*3) = "fizz"
multiplesOf3AreFizz3 (Z ** pf) {n} = case n of
                                      Z => ?multiplesOf3AreFizz3_rhs_1
                                      (S k) => ?multiplesOf3AreFizz3_rhs_4
multiplesOf3AreFizz3 ((S k) ** pf) = ?multiplesOf3AreFizz3_rhs_3

