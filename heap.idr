import Data.List.Views
import Specdris.Spec

%default total

mutual
  data BBT : Type where
    EmptyTree: BBT 
    Leaf : Integer -> BBT 
    Chunk : Integer -> (x: BBT ) ->  (y: BBT ) -> {auto p: (depth x) = (depth y)} ->  BBT 
  
  depth : BBT -> Nat
  depth EmptyTree = 0
  depth (Leaf x) = 1
  depth (Chunk x y z) = 1 + ( max (depth y) (depth z))

Show (BBT ) where 
  show EmptyTree = "Empty Tree"
  show (Leaf n) = show n
  show (Chunk n bbt1 bbt2) = show n ++ " (" ++ show bbt1 ++ ") " ++  " (" ++ show bbt2 ++ ") "

Eq (BBT ) where 
  (==) EmptyTree EmptyTree  = True
  (==) (Leaf x) (Leaf y)  = x == y 
  (==) (Chunk x y z) (Chunk w s t) = x == w && y == s && z == t
  (==) _ _ = False


-- TODO make total (recursion)
treeFromList : List Integer -> BBT 
treeFromList [] = EmptyTree
treeFromList (x :: []) = Leaf x
treeFromList (x :: (y :: [])) = ?lksjf
treeFromList (x :: xs) = treeFromListHelper  (x :: xs)
where
  treeFromListHelper : List Integer -> BBT
  treeFromListHelper xs with (splitBalanced xs)
   treeFromListHelper (ys ++ zs) | (MkSplitBal y) = 
     let ll = assert_total $ treeFromList ys
         lr = assert_total $ treeFromList zs in
         case decEq (depth ll) (depth lr) of 
              (Yes prf) => Chunk x ll lr
              (No contra) => ?kjsdj_2

main: IO ()
main = spec $ do 
  describe "balanced binary tree" $ do
    it "treeFromList with empty list is empty tree" $ do
      treeFromList [] `shouldBe` EmptyTree
    it "treeFromList with one element" $ do
      treeFromList [1] `shouldBe` Leaf 1
    it "treeFromList with three elements" $ do
      treeFromList [1, 2, 3] `shouldBe` Chunk (1) (Leaf 2) (Leaf 3)
    it "treeFromList with two elements" $ do
      treeFromList [1..7] `shouldBe` Chunk (1) (Chunk 2 (Leaf 3) (Leaf 4)) (Chunk 5 (Leaf 6) (Leaf 7))

emptyTree : BBT 
emptyTree = EmptyTree

incorrectTree : BBT
-- incorrectTree = Chunk 1 (Chunk 2 (Leaf 3) EmptyTree) EmptyTree


-- toto : Integer -> String
-- toto n = case (decEq n 3) of 
--               (Yes prf) => ?kjsdfj_1
--               (No contra) => counterProof contra

-- abzurd : (Vect 2 Integer -> BBT 3) -> Void
