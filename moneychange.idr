import Data.Vect

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  SucChange : (prev: Change amount) -> Change (S amount) 
  Simple : (value: Nat) -> (quantity: Nat) -> Change (value * quantity)

change : (amount: Nat) -> Change amount
change Z = NoChange
change (S k) = SucChange (change k)

data CompositeChange : (totalAmount: Nat) -> Type where
  Composite :  (ch1: Change a1) -> (ch2: Change a2) -> CompositeChange (a1+a2)
  
constrainedComposite : (amount: Nat) -> CompositeChange amount 
constrainedComposite amount = ?hole -- Composite (Simple amount (S Z)) (Simple Z Z)
-- constrainedChange amount = ?constrainedChange_rhs

fourCent : Change 4 
fourCent = Simple 2 2

eightCent : Change 8 
eightCent = Simple 2 4


someComposite : CompositeChange 12
someComposite = Composite fourCent eightCent



