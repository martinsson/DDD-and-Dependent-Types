--%default total

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  NextCoin : (value: Nat) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount} -> 
             Change amount

Show (Change amount) where
  show NoChange = "0"
  show (NextCoin value prev) = show value ++ " + " ++ show prev 

removeN : (n: Nat) -> (k: Nat) -> (prf : n `LTE` k) -> (result ** n + result = k)
removeN Z k prf = (k ** Refl ) 
removeN (S j) (S k) (LTESucc prf) =
  let (result ** prevProof) = removeN j k prf
  in  (result ** rewrite eqSucc (j + result) k prevProof in Refl )

-- 1. Total of all the coins returns is the input amount
-- 2. There is no other solution for this number where there are less coins

change : (amount: Nat) -> Change amount
change Z = NoChange
change (S Z) = NextCoin 1 (change Z) 
change (S (S k)) = let coinValue = (the Nat 5)
                   in case (coinValue `isLTE` (S (S k))) of
                           (Yes prf) => let (remainingAmount **  amountPrf) = removeN coinValue (S (S k)) prf  
                                        in NextCoin coinValue (change remainingAmount) {prf = amountPrf} 
                           (No contra) => NextCoin 1 (change (S k)) 

main : IO()
main = putStrLn $ show (change 15)



