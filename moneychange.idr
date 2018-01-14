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


change : (amount: Nat) -> Change amount
change Z = NoChange
change (S k) = rewrite (sym $ plusZeroRightNeutral k) in 
                       Composite (SucChange (change k)) NoChange


-- change2 : (amount: Nat) -> Change
