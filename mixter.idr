import Data.List

UserId: Type
UserId = String

||| a tweet has a userId - the author
data Tweet: (userId: UserId) -> Type where 
  MkTweet: (userId: UserId) -> String -> Tweet userId

||| Command emitted by user and parsed by a router
data DeleteCommand: (userId: UserId) -> Type where
  MkDeleteCommand: (userId: UserId) -> DeleteCommand userId  

||| Only the author of a tweet can emit this event, that's enforced
||| by the fact that in order to instantiate this event we need to provide both
||| a Tweet and a Deletecommand having the same userId. 
||| The calling code needs to show that they are identical, otherwise it won't compile
data TweetDeleted: Type where
  MkTweetDeleted: (DeleteCommand userId) -> (Tweet userId) 
                    -> TweetDeleted  

||| this would be the public function of the message aggregate
||| it decides whether a TweetDeleted event should be emitted
||| the tweet with an authorId would come from some persistence layer
||| the delete command from an external request.
emitTweetDeleted : Tweet authorId -> DeleteCommand userId -> Maybe TweetDeleted
emitTweetDeleted tweet@(MkTweet authorId _) (MkDeleteCommand userId) = 
                      case decEq authorId authorId of
                           -- in the just case there should be a way of using the existing cmd
                          (Yes prf) => Just $ MkTweetDeleted (MkDeleteCommand authorId) tweet 
                          (No contra) => Nothing

||| basic example of creating the event
myDeletionEvent : TweetDeleted
myDeletionEvent = let cmd =  MkDeleteCommand "Emilien"
                      msg = MkTweet "Emilien" "Type Driven Development rocks!" in
                  MkTweetDeleted cmd msg 
