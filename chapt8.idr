import Data.Vect

data EqNat : (n1: Nat) -> (n2: Nat) -> Type where
  Same : (num: Nat) -> EqNat num num

sameS : (x : EqNat k j) -> Maybe (EqNat (S k) (S j))
sameS (Same n) = Just (Same (S n))

checkEqNat : (n1: Nat) -> (n2: Nat) -> Maybe (EqNat n1 n2)
checkEqNat Z Z = Just (Same _)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              (Just x) => sameS x


exactLength : (len: Nat) -> (input: Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat len m of
                                 Nothing => Nothing
                                 (Just (Same m)) => Just input

