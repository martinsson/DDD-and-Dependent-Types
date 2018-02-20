--%default total
data Coin  : (value: Nat) -> Type where
  OneCent  : Coin 1
  FiveCent : Coin 5

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  NextCoinn : (coin: Coin value) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount} -> 
             Change amount


showCoinVal : (coin : Coin value) -> String
showCoinVal coin {value} = show value 

Show (Change amount) where
  show NoChange = "0"
  show (NextCoinn coin prev) = showCoinVal coin ++ " + " ++ show prev 

removeN : (n: Nat) -> (k: Nat) -> (prf : n `LTE` k) -> (result ** n + result = k)
removeN Z k prf = (k ** Refl ) 
removeN (S j) (S k) (LTESucc prf) =
  let (result ** prevProof) = removeN j k prf
  in  (result ** rewrite eqSucc (j + result) k prevProof in Refl )

-- 1. Total of all the coins returns is the input amount
-- 2. There is no other solution for this number where there are less coins

changeHelper : (amount: Nat) -> Coin coinValue -> Change amount
changeHelper Z c = NoChange
changeHelper (S Z) c = NextCoinn OneCent NoChange
changeHelper (S (S k)) coin {coinValue} = 
                   case (coinValue `isLTE` (S (S k))) of
                           (Yes lteProof) => appendCoinOf coin (S (S k)) lteProof  
                           (No contra) => NextCoinn OneCent (changeHelper (S k) OneCent)
       where 
         appendCoinOf: (coin: Coin value) -> (amount: Nat) -> (lteProof: value `LTE` amount) -> Change amount
         appendCoinOf coin {value} amount lteProof =  let (remainingAmount **  prf) = removeN value amount lteProof  
                                                          remainingChange = changeHelper remainingAmount coin
                                                          in NextCoinn coin remainingChange
 

change : (amount: Nat) -> Change amount
change amount = changeHelper amount FiveCent 

giveChangeOf : Nat -> String
giveChangeOf k = (show (change k))


main : IO()
main = putStrLn $ show (change 15)



