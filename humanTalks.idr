import Data.Vect

--// types first => append

append : List a -> List a -> List a



























modulo : (dividend: Nat) -> (divisor: Nat) -> Nat
modulo dividend (S k) = ?modulo_rhs_2














data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  NextCoinn : (coin: Coin value) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount} -> 
             Change amount

change : (amount: Nat) -> Change amount









data Coin  : (value: Nat) -> Type where
  OneCent  : Coin 1
  FiveCent : Coin 5


