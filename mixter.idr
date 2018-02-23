UserId: Type
UserId = String

data Message = Msg UserId String
data DeleteCommand = DelCmd UserId   

data Event = DeleteEvent DeleteCommand Message  
           

cmd : DeleteCommand
cmd = DelCmd "Emilien"

msg : Message
msg = Msg "Florent" "on est bien là"

myEvent : Event
myEvent = DeleteEvent cmd msg
