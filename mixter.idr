UserId: Type
UserId = String

data Message: (userId: UserId) -> Type where 
  MkMessage: (userId: UserId) -> String -> Message userId
data DeleteCommand: (userId: UserId) -> Type where
  MkDeleteCommand: (userId: UserId) -> DeleteCommand userId  

-- command.author = msg.userId
data Event : Type where 
  DeleteEvent: (DeleteCommand userId) -> (Message userId) -> Event  
           
cmd : DeleteCommand "Florent"
cmd = MkDeleteCommand "Florent"

msg : Message "Emilien"
msg = MkMessage "Emilien" "on est bien là"

myEvent : Event
myEvent = DeleteEvent cmd msg
