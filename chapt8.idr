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

same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys 
same_cons prf = cong prf

same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl  
-- this solves itself, but I don't get this code


data ThreeEq : a -> b -> c -> Type where
     AllSame : ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x AllSame = AllSame 

myPlusCommutes : (n: Nat) -> (m: Nat) -> n + m = m + n
myPlusCommutes Z m =  rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in 
                             rewrite plusSuccRightSucc  m k in Refl
                             
reverse_proof_ys : Vect ((S n) + len) a -> Vect (plus n (S len)) a
reverse_proof_ys {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs
                             
myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' {n} acc [] = rewrite plusZeroRightNeutral n in  acc
        reverse' {n} acc (y :: ys) = reverse_proof_ys (reverse' (y::acc) ys)

