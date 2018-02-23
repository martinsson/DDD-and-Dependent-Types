UserId: Type
UserId = String


data Message: (userId: UserId) -> Type where 
  MkMessage: (userId: UserId) -> String -> Message userId
data DeleteCommand: (userId: UserId) -> Type where
  MkDeleteCommand: (userId: UserId) -> DeleteCommand userId  


data MessageCreated: Type where
  MkMessageCreated: UserId -> MessageCreated

data MessageDeleted: Type where
  MkMessageDeleted: (DeleteCommand userId) -> (Message userId) -> MessageDeleted  

data Event =  Deleted MessageDeleted | Created MessageCreated

History: Type
History = List Event

mkEvent: DeleteCommand userId -> Message userId -> MessageDeleted
mkEvent command message = MkMessageDeleted command message


cmd : DeleteCommand "Emilien"
cmd = MkDeleteCommand "Emilien"

msg : Message "Emilien"
msg = MkMessage "Emilien" "on est bien là"

myEvent : MessageDeleted
myEvent = MkMessageDeleted cmd msg
