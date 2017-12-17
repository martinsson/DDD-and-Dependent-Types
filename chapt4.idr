import Data.Vect

data DataStore : Type where
  MkData : (size: Nat) -> (items: Vect size String) -> DataStore


addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
   where
     addToData : Vect old String -> Vect (S old) String
     addToData xs = newitem :: xs

data Command = Add String 
             | Get Integer
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand cmd args = ?parseCommand_rhs

parse : (input: String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)


process_command : DataStore -> String -> Maybe (String, DataStore)
process_command ds command = ?hole_rhs

main : IO ()
main = replWith (MkData _ []) "Command: " process_command


