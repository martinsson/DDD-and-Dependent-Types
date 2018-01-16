%default total

-- 1. Total of all the coins returns is the input amount
-- 2. There is no other solution for this number where there is less coins

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  NextCoin : (value: Nat) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount'} -> 
             Change amount'


extractAmount : (Change amount) -> Nat
extractAmount {amount} _ = amount

Eq (Change amount) where
  (==) x y = (extractAmount x) == (extractAmount y)


change : (amount: Nat) -> Change amount
change Z = NoChange
change (S Z) = NextCoin 1 (change Z) 
change (S (S k)) = NextCoin 2 (change k) 

-- calculateTotal : List Coin -> Nat
-- change' : (amount: Nat) -> (calculateTotal coins = amount ** coins : List Coin)


-- change2 : (amount: Nat) -> Change
