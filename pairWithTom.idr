
-- diversion, money change, fizzbuzz (divisible by)

-- Learnings: 
-- Tom: I understand this, until(!) I implement it
--      functions that take a proof, ensure that they're never called without that proof and
--      the arguements to the proof
--      For instance constructors


-- Johan: Constructing a proof was new to me
--        using a if then clause in a type




import Data.Vect

toto : (b: Bool) -> if b then String else Nat
toto False = 0
toto True = ""

data FizzT = Fizz

isFizz : Nat -> Bool
isFizz (S (S (S k))) = isFizz k
isFizz Z = True
isFizz _ = False

data FBT : (a : Nat) -> Type where
  FizzC : (a : Nat) -> (a = 3) -> FBT a
  Number : FBT a


fb : (a : Nat) -> FBT a
fb a = case decEq a 3 of
            Yes prf => FizzC a prf
            No contra => Number

fb2 : (a : Nat) -> String
fb2 x = case fb x of
             (FizzC x prf) => "fizz"
             Number => show x

FizzBuzz : (a : Nat) -> Type
FizzBuzz a = if isFizz a then FizzT else String

fizzbuzz : (a: Nat) -> FizzBuzz a
fizzbuzz Z = Fizz
fizzbuzz (S Z) = "1" 
fizzbuzz (S (S Z)) = "2"
fizzbuzz (S (S (S k))) = fizzbuzz k


-- is3Proof : (a : Nat) -> Dec (a = 3)
-- is3Proof (S (S (S Z))) = Yes Refl
-- is3Proof _ = No notThree

is3Proof : (a : Nat) -> Dec (a = 3)
is3Proof a = decEq a 3
-- is3Proof a = case decEq a 3 of
--                   Yes prf => Yes prf
--                   No contra => No contra

