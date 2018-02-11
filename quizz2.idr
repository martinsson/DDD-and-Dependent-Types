module Quizz

import Data.String 
import Data.Vect
import Data.Fin

data Question : Type where
   QCM : {numOptions : Nat}
      -> (question : String) 
      -> (qcmOptions : Vect numOptions String)
      -> (expected : Fin numOptions)
      -> Question

data Answer : (q : Question) -> (m: Nat) -> Type where
  AnswerQCM : (option : Fin n) -> 
              (prf: finToNat option = m) ->
              Answer (QCM {numOptions = n } q opts exp) m
  
couldntHappen : (x: Fin numOptions) -> ((finToNat x = n) -> Void) -> ( Answer (QCM {numOptions} question qcmOptions expected) n -> Void)

validateAnswer : (n : Nat) -> (q : Question) -> Dec (Answer q n)
validateAnswer n (QCM {numOptions} question qcmOptions expected) =
 (case natToFin n numOptions of
       Nothing => No ?slkjdf_1
       (Just x) => case decEq (finToNat x) n of  -- this is absurd, x was made from n sot they are eqaual
                        (Yes prf) => Yes (AnswerQCM x prf) -- here we're happy as we have the proof
                        (No contra) => No (couldntHappen x contra))  -- here we have a contra proof of pretty much the proof AnswerQCM takes 


