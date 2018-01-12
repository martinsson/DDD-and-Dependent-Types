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

lte_rhs_2 : (contra : LTE k j -> Void) -> LTE (S k) (S j) -> Void
lte_rhs_2 contra (LTESucc x) = contra x


lte: (b: Nat) -> (c: Nat) -> Dec (LTE b c)
lte Z Z = Yes LTEZero
lte Z (S k) = Yes LTEZero
lte (S k) Z = No succNotLTEzero
lte (S k) (S j) = case Main.lte k j of
                       (Yes prf) => Yes (LTESucc prf)
                       (No contra) => No (lte_rhs_2 contra)

multEqual : (a: Nat) -> (b: Nat) -> (c: Nat) -> Dec (a * b = c)
-- multEqual Z b Z = Yes Refl
-- multEqual a Z Z = rewrite multZeroRightZero a in Yes Refl
-- multEqual Z b (S k) = No multZeroNotGT0 
-- multEqual (S k) Z (S j) = No ?sdflksjfmultZeroNotGT0Right
-- multEqual (S k) (S j) Z = ?multEqual_rhs_3
-- multEqual (S k) (S j) (S i) = ?multEqual_rhs_2
multEqual Z b c = ?multEqual_rhs_1
multEqual (S Z) b c = case decEq b c of
                           (Yes prf) => rewrite plusZeroRightNeutral b in (Yes prf)
                           (No contra) => rewrite plusZeroRightNeutral b in No contra
multEqual (S (S k)) b c = case lte b c of
                               (Yes prf) => let rec = multEqual (S k) b (c - b) in ?multEqual_rhs_4
                               (No contra) => ?sksdfkjs

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



