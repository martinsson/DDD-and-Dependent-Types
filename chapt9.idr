-- import Data.List

data Elem : (el: a) -> List a -> Type where
  Here : Elem el (el :: xs)
  There : (later: Elem el xs) -> Elem el (x :: xs)


data Last : List a -> a -> Type where
  LastOne: Last [value] value
  LastCons: (prf: Last xs value) -> Last (x :: xs) value
  
isNotLast : (contra : (value = x) -> Void) -> Last [x] value -> Void
isNotLast contra LastOne = contra Refl
isNotLast _ (LastCons LastOne) impossible
isNotLast _ (LastCons (LastCons _)) impossible
  
  
isNotCons : (contra : Last (y :: xs) value -> Void) -> Last (x :: (y :: xs)) value -> Void
isNotCons contra (LastCons prf) = contra prf
  
lastNotNil : Last [] value -> Void
lastNotNil LastOne impossible
lastNotNil (LastCons _) impossible
  
isLast : (DecEq a) => (xs: List a) -> (value: a) -> Dec (Last xs value)
isLast [] value = No lastNotNil 
isLast (x :: []) value = case decEq value x of
                              (Yes Refl) => Yes LastOne
                              (No contra) => No (isNotLast contra)
isLast (x :: (y :: xs)) value = case isLast (y::xs) value of
                                     (Yes prf) => Yes (LastCons prf)
                                     (No contra) => No (isNotCons contra)


-- last123 : Last 3 [1, 2, 3]
-- last123 = LastCons (LastCons LastOne)

