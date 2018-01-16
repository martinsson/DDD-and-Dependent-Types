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

removeN : (n: Nat) -> (k: Nat) -> (prf : n `LTE` k) -> (result ** result + n = k)
removeN Z k prf = (k ** rewrite plusZeroRightNeutral k in Refl)
removeN (S j) (S k) (LTESucc prf) =
  let (result ** prevProof) = removeN j k prf in
      (result ** rewrite sym $ plusSuccRightSucc result j in 
                 rewrite eqSucc (result + j) k prevProof in Refl)

change : (amount: Nat) -> Change amount
change Z = NoChange
change (S Z) = NextCoin 1 (change Z) 
change amount@(S (S k)) = case (5 `isLTE` amount) of
                               (Yes prf) => let (result ** amountPrf) = removeN 5 amount prf  
                                in ?holeee
                               (No contra) => ?sjdf_3

--result + j = k -> result (S j) = (S k)
--(S (plus result j)) = S k




-- calculateTotal : List Coin -> Nat
-- change' : (amount: Nat) -> (calculateTotal coins = amount ** coins : List Coin)


-- change2 : (amount: Nat) -> Change
