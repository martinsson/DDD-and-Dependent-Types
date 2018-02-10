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

data Answer : (q : Question) -> (option: Fin n) -> Type where
  AnswerQCM   : (option : Fin n) -> Answer (QCM {numOptions = n } q opts exp) option

lteIsFin : (LTE a b) -> Fin b

notLTENotFin : (LTE a b -> Void) -> (Fin b -> Void)
notLTENotFin f FZ = ?notLTENotFin_rhs_2
notLTENotFin f (FS x) = ?notLTENotFin_rhs_3

notUbNoAnswer :  ((ub: Fin numOpttions) -> Void) -> 
               ( Answer (QCM {numOptions} question qcmOptions expected) ub  -> Void)

validateAnswer : (n : Nat) -> (q : Question) -> Dec (Answer q ub)
validateAnswer n (QCM {numOptions} question qcmOptions expected) = 
  (case n `isLTE` numOptions of
        (Yes prf) => Yes ?sdfjj -- (AnswerQCM ?ksjdf )
        (No contra) => let fin = notLTENotFin contra 
                           -- noAnwser = notUbNoAnswer fin 
                           in No ?noAnswer)

