import Data.List

UserId: Type
UserId = String

data Message: (userId: UserId) -> Type where 
  MkMessage: (userId: UserId) -> String -> Message userId
data DeleteCommand: (userId: UserId) -> Type where
  MkDeleteCommand: (userId: UserId) -> DeleteCommand userId  


mutual
  data MessageCreated: Type where
    MkMessageCreated: UserId -> MessageCreated
  
  data MessageDeleted: Type where
    MkMessageDeleted: (DeleteCommand userId) -> (Message userId) 
                      -> (history: History)
                      -> IsNotDeleted history 
                      -> MessageDeleted  

  data Event =  Deleted MessageDeleted | Created MessageCreated
  
  History : Type
  History = List Event

  IsNotDeleted : (history : History) -> Type 
  IsNotDeleted history = Not (length history = 2)

  
isNotDeleted : (history : List Event) -> Dec (IsNotDeleted history)
isNotDeleted history =  decEq (Not (length history) 2)
  
mkEvent: DeleteCommand userId -> Message userId -> History ->  Maybe MessageDeleted
mkEvent command message history = case (isNotDeleted history)  of
                                   (Yes prf) => Just (MkMessageDeleted command message history prf)
                                   (No contra) => Nothing

cmd : DeleteCommand "Emilien"
cmd = MkDeleteCommand "Emilien"

msg : Message "Emilien"
msg = MkMessage "Emilien" "on est bien là"

myEvent : MessageDeleted
myEvent = mkEvent cmd msg [] 
