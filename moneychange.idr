import Data.Vect

OneP : Nat
OneP = 1
TwoP : Nat
TwoP = 2

-- data Shange = Nat -> CoinType -> Nat -> Type where
--   SimpleShange : CoinType -> Nat 
data Change : (amount: Nat) -> Type where
  Simple : (value: Nat) -> (quantity: Nat) -> Change (value * quantity)

data CompositeChange : (totalAmount: Nat) -> Type where
  Composite :  (ch1: Change a1) -> (ch2: Change a2) -> CompositeChange (a1+a2)

constrainedChange : (amount: Nat) -> CompositeChange amount 
-- constrainedChange amount = Composite ?sdf ?slkdfjlskf

someChange : Change 4 
someChange = Simple 2 2

someChange2 : Change 8 
someChange2 = Simple 2 4


someComposite : CompositeChange 12
someComposite = Composite someChange someChange2

-- data Shange (n: Nat) -> (m: Nat) -> (amount: Nat) -> Type where
  
-- changeOptions : (amount: Nat) -> Change 

-- data Change = OnePence Nat | TwoPence Nat | Ch Change Change
            
-- data Change : Nat where
-- OnePence : (count : Nat) -> Change 1
-- TwoPence : (count : Nat) -> Change 2



-- changeFor : (amount: Nat) -> List 


