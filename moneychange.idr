import Data.Vect

OneP : Nat
OneP = 1
TwoP : Nat
TwoP = 2

-- data Shange = Nat -> CoinType -> Nat -> Type where
--   SimpleShange : CoinType -> Nat 
data Change : (value: Nat) -> (quantity: Nat) -> (amount: Nat) -> Type where
  Simple : (value: Nat) -> (quantity: Nat) -> Change value quantity (value * quantity)

data CompositeChange : Change v1 q1 a1 -> Change v2 q2 a2 -> (totalAmount: Nat) -> Type where
  Composite :  (ch1: Change v1 q1 a1) -> (ch2: Change v2 q2 a2) -> CompositeChange ch1 ch2 (a1+a2)

constrainedChange : (amount: Nat) -> CompositeChange change1 change2 amount 
-- constrainedChange amount = Composite ?sdf ?slkdfjlskf

someChange : Change value quantity 4 
someChange = Simple 2 2

-- data Shange (n: Nat) -> (m: Nat) -> (amount: Nat) -> Type where
  
-- changeOptions : (amount: Nat) -> Change 

-- data Change = OnePence Nat | TwoPence Nat | Ch Change Change
            
-- data Change : Nat where
-- OnePence : (count : Nat) -> Change 1
-- TwoPence : (count : Nat) -> Change 2



-- changeFor : (amount: Nat) -> List 


