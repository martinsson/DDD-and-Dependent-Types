%default total

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  SucChange : (prev: Change amount) -> Change (S amount) 
  Composite : (ch1: Change amount1) -> (ch2: Change amount2) ->  
              {auto prf: amount1 + amount2 = amount'}-> Change amount'
--  Simple : (value: Nat) -> (quantity: Nat) -> 
--           {auto prf: value * quantity = amount'} -> Change amount' 


extractAmount : (Change amount) -> Nat
extractAmount {amount} _ = amount

Eq (Change amount) where
  (==) x y = (extractAmount x) == (extractAmount y)



multZeroNotGT0 : (0 = S k) -> Void
multZeroNotGT0 Refl impossible

multZeroNotGT0Right : (mult k 0 = S j) -> Void
multZeroNotGT0Right {k} prf = ?sdkfjs --rewrite (sym $ multZeroRightZero k) in ?sldkjf


change : (amount: Nat) -> Change amount
change Z = NoChange
change (S k) = SucChange (change k) 


