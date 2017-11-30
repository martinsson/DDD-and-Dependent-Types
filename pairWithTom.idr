
-- diversion, money change, fizzbuzz (divisible by)

import Data.Vect

toto : (b: Bool) -> if b then String else Nat
toto False = 0
toto True = ""

-- fizzbuzz : Nat -> Fizz
-- fizzbuzz k = ?fizzbuzz_rhs

-- data Fizz = FizzZ Z 
--          | FizzRest (S (S (S Nat)))

-- twoNatsAreEq : (a : Nat) -> Dec (isDivisibleBy 3 a )
data FizzT = Fizz

isFizz : Nat -> Bool
isFizz (S (S (S k))) = isFizz k
isFizz Z = True
isFizz _ = False

data FBT : (a : Nat) -> Type where
  FizzC : (a : Nat) -> (isFizz a = True) -> FBT a
  Number : FBT a

-- fb : (a : Nat) -> FBT a
-- fb a = case decEq a 3 of
            -- Yes prf => FizzC a prf
            -- No contra => Number

fb : (a : Nat) -> FBT a
fb a = case decEq (a % 3 == 0) True of
            Yes prf => FizzC a prf
            No contra => Number

FizzBuzz : (a : Nat) -> Type
FizzBuzz a = if isFizz a then FizzT else String

fizzbuzz : (a: Nat) -> FizzBuzz a
fizzbuzz Z = Fizz
fizzbuzz (S Z) = "1" 
fizzbuzz (S (S Z)) = "2"
fizzbuzz (S (S (S k))) = fizzbuzz k


-- notThree : {a : Nat} -> (a = 3) -> Void
-- notThree Refl impossible

-- is3Proof : (a : Nat) -> Dec (a = 3)
-- is3Proof (S (S (S Z))) = Yes Refl
-- is3Proof _ = No notThree

is3Proof : (a : Nat) -> Dec (a = 3)
is3Proof a = decEq a 3
-- is3Proof a = case decEq a 3 of
--                   Yes prf => Yes prf
--                   No contra => No contra

