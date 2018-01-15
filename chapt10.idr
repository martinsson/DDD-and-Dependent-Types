import Data.Vect 

data TakeN : List a -> Type where
  Fewer : TakeN xs
  Exact : (n_xs: List a) -> TakeN (n_xs ++ rest)

takeN : (n: Nat) -> (xs: List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S k) (x :: xs) | Fewer = Fewer
  takeN (S k) (x :: (n_xs ++ rest)) | (Exact n_xs) = Exact (x::n_xs) 


-- I'm unhappy with this as it leaves an empty array in the end. ex
-- > groupByN 2 [1..4]
-- [[1, 2], [3, 4], []] : List (List Integer)
groupByN: (n: Nat) -> (xs: List a) -> List (List a)
groupByN n xs  with (takeN n xs)
  groupByN n xs  | Fewer = [xs]
  groupByN n (n_xs ++ rest)  | (Exact n_xs) = n_xs :: groupByN n rest

halvesHelp : (xs : List a) -> Nat -> (List a, List a)
halvesHelp xs Z = ([], [])
halvesHelp xs (S k) with (takeN (S k) xs)
  halvesHelp xs (S k) | Fewer = halvesHelp xs k
  halvesHelp (n_xs ++ rest) (S k) | (Exact n_xs) = (n_xs, rest)

halves : List a -> (List a, List a)
halves xs = halvesHelp xs (div (length xs) 2) 

halvesVect : (xs: List a) -> (Vect (div (length xs) 2) a )
halvesVect xs = ?lskdjf -- Vect.Nil :: (take (div (length xs) 2) xs)
