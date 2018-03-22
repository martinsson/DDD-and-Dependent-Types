import Specdris.Spec

%default total

data BBT = EmptyTree

emptyTree : BBT
emptyTree = EmptyTree

Show BBT where 
  show EmptyTree = "Empty Tree"

Eq BBT where 
  (==) _ _  = True 

treeFromList : List Integer -> BBT

main: IO ()
main = spec $ do 
  describe "balanced binary tree" $ do
    it "treeFromList with empty list is empty tree" $ do
      treeFromList [] `shouldBe` EmptyTree
