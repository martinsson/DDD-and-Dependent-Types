import Specdris.Spec

%default total

data BBT = EmptyTree | Leaf Integer | Chunk Integer BBT BBT

Show BBT where 
  show EmptyTree = "Empty Tree"
  show (Leaf n) = show n
  show (Chunk n bbt1 bbt2) = show n ++ " (" ++ show bbt1 ++ ") " ++  " (" ++ show bbt2 ++ ") "

Eq BBT where 
  (==) EmptyTree EmptyTree  = True
  (==) (Leaf x) (Leaf y)  = x == y 
  (==) (Chunk x y z) (Chunk w s t) = x == w && y == s && z == t
  (==) _ _ = False

treeFromList : List Integer -> BBT
treeFromList [] = EmptyTree
treeFromList (x :: []) = Leaf x
treeFromList (x :: (y :: [])) = ?lksjf
treeFromList (x :: (y :: (z :: xs))) = Chunk x (Leaf y) (Leaf z)

main: IO ()
main = spec $ do 
  describe "balanced binary tree" $ do
    it "treeFromList with empty list is empty tree" $ do
      treeFromList [] `shouldBe` EmptyTree
    it "treeFromList with one element" $ do
      treeFromList [1] `shouldBe` Leaf 1
    it "treeFromList with three elements" $ do
      treeFromList [1, 2, 3] `shouldBe` Chunk (1) (Leaf 2) (Leaf 3)


emptyTree : BBT 
emptyTree = EmptyTree

incorrectTree : BBT
incorrectTree = Chunk 1 (Chunk 2 (Leaf 3) EmptyTree) EmptyTree


counterProof : (contra : (n = fromInteger 3) -> Void) -> String

-- toto : Integer -> String
-- toto n = case (decEq n 3) of 
--               (Yes prf) => ?kjsdfj_1
--               (No contra) => counterProof contra

-- abzurd : (Vect 2 Integer -> BBT 3) -> Void
