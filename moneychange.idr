module moneychange
import Data.String

--%default total
data Coin  : (value: Nat) -> Type where
  OneCent  : Coin 1
  FiveCent : Coin 5
  TenCent  : Coin 10

getVal : Coin v -> Nat
getVal _ {v} = v

Eq (Coin v) where
  (==) x y = getVal x == getVal y

data CoinT : Type where
  MkCoinT : Coin v -> CoinT

getCoinTval : CoinT -> Nat 
getCoinTval (MkCoinT x) = getVal x

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  NextCoin : (coin: Coin value) -> (prev: Change prevAmount) -> 
             {auto prf: value + prevAmount = amount} -> 
             Change amount
             
groupByCoinType : Change amount -> List (Nat, CoinT) 
groupByCoinType change = groupByHelper change [] 
where
  groupByHelper : Change a -> List (Nat, CoinT) -> List (Nat, CoinT)
  groupByHelper NoChange groups = groups
  groupByHelper (NextCoin coin prev) [] = groupByHelper prev [(1, MkCoinT coin)]
  groupByHelper (NextCoin coin prev) ((quantity, coinT) :: xs) =  if getVal coin == getCoinTval coinT 
                                                                     then groupByHelper prev ((quantity+1, coinT) :: xs)
                                                                     else groupByHelper prev ((1, MkCoinT coin) :: (quantity, coinT) :: xs)


Show (Coin v) where
  show x {v} = show v

Show (Change amount) where
  show NoChange = "0"
  show (NextCoin coin prev) = show coin ++ " + " ++ show prev 

removeN : (n: Nat) -> (k: Nat) -> (prf : n `LTE` k) -> (result ** n + result = k)
removeN Z k prf = (k ** Refl ) 
removeN (S j) (S k) (LTESucc prf) =
  let (result ** prevProof) = removeN j k prf
  in  (result ** rewrite eqSucc (j + result) k prevProof in Refl )

-- 1. Total of all the coins returns is the input amount
-- 2. There is no other solution for this number where there are less coins

changeHelper : (amount: Nat) -> Coin coinValue -> List CoinT -> Change amount 
changeHelper (S k) coin [] = NextCoin OneCent (changeHelper k OneCent []) 
changeHelper (S k) coin {coinValue} (x@(MkCoinT smallerCoin) :: xs) = 
                   case (coinValue `isLTE` (S k)) of
                           (No contra) => changeHelper (S k) smallerCoin xs
                           (Yes lteProof) => appendCoinOf coin (S k) lteProof  
       where 
         appendCoinOf: (coin: Coin value) -> (amount: Nat) -> (lteProof: value `LTE` amount) -> Change amount
         appendCoinOf coin {value} amount lteProof =  let (remainingAmount **  prf) = removeN value amount lteProof  
                                                          remainingChange = changeHelper remainingAmount coin (x :: xs) 
                                                          in NextCoin coin remainingChange
 

-- Learning: if I don't lift the coin to the type level like in changeHelper then I cannot use the value inside 
-- as it will not unify with the One- or Five-Cent coin, in particular hte lteProof will not unify

allCoinT : List CoinT
allCoinT = [MkCoinT TenCent, MkCoinT FiveCent, MkCoinT OneCent]

-- Learning: using where clauses allows for nice private methods, defined after the call
-- Learning: parsePositive takes the type as an implicit parameter, great!
change : (amount: Nat) -> Change amount
change amount = changeHelper amount TenCent allCoinT  

main : IO()
main = do 
  putStrLn "what's the required amount?"
  x <- getLine 
  case parsePositive {a=Nat} x of
       Nothing => putStrLn "please provide a positive integer"
       (Just x) => putStrLn $ "The change is " ++ show (change x)
  main



