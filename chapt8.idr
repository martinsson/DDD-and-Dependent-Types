import Data.Vect

checkEqNat : (n1: Nat) -> (n2: Nat) -> Maybe (n1 = n2)
checkEqNat Z Z = Just Refl
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just prf => Just (cong prf)
                              -- why on earth do we need the case (S k) (S j)?
                              


exactLength : (len: Nat) -> (input: Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat len m of
                                 Nothing => Nothing
                                 (Just Refl) => Just input

