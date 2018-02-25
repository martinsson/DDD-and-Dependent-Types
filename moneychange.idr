import Data.String

--%default total
data Coin  : (value: Nat) -> Type where
  OneCent  : Coin 1
  FiveCent : Coin 5

data CoinT : Type where
  MkCoinT : Coin v -> CoinT

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  NextCoin : (coin: Coin value) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount} -> 
             Change amount

showCoinVal : (coin : Coin value) -> String
showCoinVal coin {value} = show value 

Show (Change amount) where
  show NoChange = "0"
  show (NextCoin coin prev) = showCoinVal coin ++ " + " ++ show prev 

removeN : (n: Nat) -> (k: Nat) -> (prf : n `LTE` k) -> (result ** n + result = k)
removeN Z k prf = (k ** Refl ) 
removeN (S j) (S k) (LTESucc prf) =
  let (result ** prevProof) = removeN j k prf
  in  (result ** rewrite eqSucc (j + result) k prevProof in Refl )

-- 1. Total of all the coins returns is the input amount
-- 2. There is no other solution for this number where there are less coins

changeHelper : (amount: Nat) -> Coin coinValue -> List CoinT -> Change amount
changeHelper Z c coins = NoChange
changeHelper (S Z) c coins = NextCoin OneCent NoChange -- Todo remove this special case
changeHelper (S (S k)) coin {coinValue} coins = 
                   case (coinValue `isLTE` (S (S k))) of
                           (Yes lteProof) => appendCoinOf coin (S (S k)) lteProof  
                           (No contra) => NextCoin OneCent (changeHelper (S k) OneCent [])
       where 
         appendCoinOf: (coin: Coin value) -> (amount: Nat) -> (lteProof: value `LTE` amount) -> Change amount
         appendCoinOf coin {value} amount lteProof =  let (remainingAmount **  prf) = removeN value amount lteProof  
                                                          remainingChange = changeHelper remainingAmount coin ?rest
                                                          in NextCoin coin remainingChange
 

-- Learning: if I don't lift the coin to the type level like in changeHelper then I cannot use the value inside 
-- as it will not unify with the One- or Five-Cent coin, in particular hte lteProof will not unify

allCoinT : List CoinT
allCoinT = [MkCoinT OneCent, MkCoinT FiveCent]

-- Learning: using where clauses allows for nice private methods, defined after the call
-- Learning: parsePositive takes the type as an implicit parameter, great!
change : (amount: Nat) -> Change amount
change amount = changeHelper amount FiveCent allCoinT  

giveChangeOf : Nat -> String
giveChangeOf k = (show (change k))


main : IO()
main = do 
  putStrLn "what's the required amount?"
  x <- getLine 
  case parsePositive {a=Nat} x of
       Nothing => putStrLn "please provide a positive integer"
       (Just x) => putStrLn $ "The change is " ++ show (change x)
  main
-- putStrLn $ show (change 15)



