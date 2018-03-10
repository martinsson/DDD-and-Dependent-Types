import Data.Vect

data Coin  : (value: Nat) -> Type where
  OneCent  : Coin 1
  FiveCent : Coin 5

data CoinT : Type where
  MkCoinT : Coin v -> CoinT

ExCoin : (v : Nat ** Coin v)
-- ExCoin = DPair Nat Coin

showCoin : CoinT -> String
showCoin (MkCoinT x {v}) = show v 

pChange : List CoinT -> String
pChange xs = concatMap showCoin xs

mkCoinPair : Coin v -> (v: Nat ** Coin v)
mkCoinPair x {v} = (v ** x)

