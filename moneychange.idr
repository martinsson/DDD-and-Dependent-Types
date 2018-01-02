%default total

data Change : (amount: Nat) -> Type where
  NoChange : Change Z
  SucChange : (prev: Change amount) -> Change (S amount) 
  Simple : (value: Nat) -> (quantity: Nat) -> 
           {auto prf: value * quantity = amount'} -> Change amount' 

extractAmount : (Change amount) -> Nat
extractAmount {amount} _ = amount

Eq (Change amount) where
  (==) x y = (extractAmount x) == (extractAmount y)



multZeroNotGT0 : (0 = S k) -> Void
multZeroNotGT0 Refl impossible

multZeroNotGT0Right : (mult k 0 = S j) -> Void
multZeroNotGT0Right {k} prf = ?sdkfjs --rewrite (sym $ multZeroRightZero k) in ?sldkjf

multEqual : (a: Nat) -> (b: Nat) -> (c: Nat) -> Dec (a * b = c)
-- multEqual Z b Z = Yes Refl
-- multEqual a Z Z = rewrite multZeroRightZero a in Yes Refl
-- multEqual Z b (S k) = No multZeroNotGT0 
-- multEqual (S k) Z (S j) = No ?sdflksjfmultZeroNotGT0Right
-- multEqual (S k) (S j) Z = ?multEqual_rhs_3
-- multEqual (S k) (S j) (S i) = ?multEqual_rhs_2
multEqual Z b c = ?multEqual_rhs_1
multEqual (S Z) b c = case decEq b c of
                           (Yes prf) => rewrite plusZeroRightNeutral b in ?multEqual_rhs_3
                           (No contra) => No ?multEqual_rhs_5
multEqual (S Z) b (S (S k)) = ?multEqual_rhs_6
multEqual (S (S k)) b c = ?multEqual_rhs_4

change : (amount: Nat) -> Change amount
change amount = rewrite (sym $ multOneRightNeutral amount) 
                    in Simple amount 1

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



