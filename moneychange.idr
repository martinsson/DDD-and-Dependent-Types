--%default total
data Coin  : (value: Nat) -> Type where
  OneCent  : Coin 1
  FiveCent : Coin 5

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  NextCoin : (value: Nat) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount} -> 
             Change amount
  NextCoinn : (coin: Coin value) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount} -> 
             Change amount


showCoinVal : (coin : Coin value) -> String
showCoinVal coin {value} = show value 

Show (Change amount) where
  show NoChange = "0"
  show (NextCoin value prev) = show value ++ " + " ++ show prev 
  show (NextCoinn coin prev) = showCoinVal coin ++ " + " ++ show prev 

removeN : (n: Nat) -> (k: Nat) -> (prf : n `LTE` k) -> (result ** n + result = k)
removeN Z k prf = (k ** Refl ) 
removeN (S j) (S k) (LTESucc prf) =
  let (result ** prevProof) = removeN j k prf
  in  (result ** rewrite eqSucc (j + result) k prevProof in Refl )

-- 1. Total of all the coins returns is the input amount
-- 2. There is no other solution for this number where there are less coins

change : (amount: Nat) -> Change amount
change Z = NoChange
change (S Z) = NextCoinn OneCent (change Z) 
change (S (S k)) = let coinValue = (the Nat 5)
                   in case (coinValue `isLTE` (S (S k))) of
                           (Yes lteProof) => appendCoinOf coinValue (S (S k)) lteProof  
                           (No contra) => NextCoinn OneCent (change (S k)) 
       where 
         appendCoinOf: (coinValue: Nat) -> (amount: Nat) -> (lteProof: coinValue `LTE` amount) -> Change amount
         appendCoinOf coinValue amount lteProof =  let (remainingAmount **  prf) = removeN coinValue amount lteProof  
                                                       remainingChange = change remainingAmount
                                                   in NextCoin coinValue remainingChange



main : IO()
main = putStrLn $ show (change 15)



