import Data.Vect

--// types first => append

append : Vect n a -> Vect m a -> Vect (n+m) a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys



























modulo : (dividend: Nat) -> (divisor: Nat) -> Nat













data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  NextCoinn : (value: Nat) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount} -> 
             Change amount

change : (amount: Nat) -> Change amount




--data Coin  : (value: Nat) -> Type where
--  OneCent  : Coin 1
--  FiveCent : Coin 5






