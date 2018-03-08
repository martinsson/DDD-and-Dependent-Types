import Data.List

UserId: Type
UserId = String

||| a tweet has a userId - the author
data Tweet: (userId: UserId) -> Type where 
  MkTweet: (userId: UserId) -> String -> Tweet userId

||| Command emitted by user and parsed by a router
data DeleteCommand: (userId: UserId) -> Type where
  MkDeleteCommand: (userId: UserId) -> DeleteCommand userId  

getUserId : (x : DeleteCommand userId) -> UserId
getUserId _ {userId}  = userId

Eq (DeleteCommand userId) where
  (==) x y = getUserId x == getUserId x 

DecEq (DeleteCommand userId) where 
  decEq (MkDeleteCommand userId) (MkDeleteCommand userId) = Yes Refl

||| Only the author of a tweet can emit this event, that's enforced
||| by the fact that in order to instantiate this event we need to provide both
||| a Tweet and a Deletecommand having the same userId. 
||| The calling code needs to show that they are identical, otherwise it won't compile
data TweetDeleted: Type where
  MkTweetDeleted: (DeleteCommand userId) -> (Tweet userId) 
                    -> TweetDeleted  

||| basic example of creating the event
myDeletionEvent : TweetDeleted
myDeletionEvent = let cmd =  MkDeleteCommand "Emilien"
                      msg = MkTweet "Emilien" "Type Driven Development rocks!" in
                  MkTweetDeleted cmd msg 

realWorldExample : Tweet authorId -> DeleteCommand userId -> Maybe TweetDeleted
realWorldExample tweet@(MkTweet authorId _) cmd@(MkDeleteCommand userId) = 
                      case decEq cmd (MkDeleteCommand authorId) of
                          (Yes prf) => ?ksjdf ---rewrite prf in Just $ MkTweetDeleted cmd tweet 
                          (No contra) => ?realWorldExample_rhs_3

data TweetCreated: Type where
  MkTweetCreated: UserId -> TweetCreated
