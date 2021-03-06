import Data.So
%default total

fizz : Nat -> Bool
fizz n = (modNatNZ n 3 SIsNotZ) == 0

data FizzT : (n: Nat) ->  Type where
  Fizz : {auto fizz : So (fizz n)} -> FizzT n
  NotFizz : {auto nfizz : So (not (fizz n)) } -> FizzT n

Show (FizzT n) where 
  show Fizz = "fizz" 
  show NotFizz = ""


buzz : Nat -> Bool
buzz n = (modNatNZ n 5 SIsNotZ) == 0

data BuzzT : (n: Nat) -> Type where
  Buzz : {auto buzz : So (buzz n)} -> BuzzT n
  NotBuzz : {auto nbuzz : So (not (buzz n)) } -> BuzzT n

Show (BuzzT n) where 
  show Buzz = "buzz"
  show NotBuzz = ""

||| the type Fizzbuzz is just an alias for the tuple of FizzT and BuzzT
Fizzbuzz: Nat -> Type
Fizzbuzz n = (FizzT n, BuzzT n)

fizzBuzz: (n: Nat) -> Fizzbuzz n
fizzBuzz n  = case (choose (fizz n), choose (buzz n)) of
             (Left _,Right _)  => (Fizz, NotBuzz) 
             (Right _,Left _)  => (NotFizz, Buzz)
             (Left _,Left _)   => (Fizz, Buzz)
             (Right _,Right _) => (NotFizz, NotBuzz)

showFizzbuzz : Nat -> String
showFizzbuzz n = let (fizzy, buzzy) = (fizzBuzz n) 
                     fizzbuzzy = (show fizzy) ++ (show buzzy) in
                     if fizzbuzzy == "" 
                        then show n 
                        else fizzbuzzy
                        

--- Tests
partial
multiplesOf3AreFizz : (n: Integer) -> showFizzbuzz (fromIntegerNat n)  = "fizz"
multiplesOf3AreFizz 3 = Refl
multiplesOf3AreFizz 6 = Refl
multiplesOf3AreFizz 9 = Refl
multiplesOf3AreFizz 12 = Refl
multiplesOf3AreFizz 18 = Refl

partial
multiplesOf5AreFizz : (n: Integer) -> showFizzbuzz (fromIntegerNat n)  = "buzz"
multiplesOf5AreFizz 5 = Refl
multiplesOf5AreFizz 10 = Refl
multiplesOf5AreFizz 20 = Refl
multiplesOf5AreFizz 25 = Refl

partial
multiplesOf3And5AreFizzbuzz : (n: Integer) -> showFizzbuzz (fromIntegerNat n)  = "fizzbuzz"
multiplesOf3And5AreFizzbuzz 15 = Refl
multiplesOf3And5AreFizzbuzz 30 = Refl

partial
nonMultiples : (n: Integer) -> showFizzbuzz (fromIntegerNat n) = (show n)
nonMultiples 1 = Refl
nonMultiples 2 = Refl
nonMultiples 37 = Refl


