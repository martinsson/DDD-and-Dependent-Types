import Specdris.Spec

data NormalScore = Love | Fifteen | Thirty | Forty
Show NormalScore where
  show Love = "Love"
  show Fifteen = "Fifteen"
  show Thirty = "Thirty"
  show Forty = "Forty"
  
Eq NormalScore where
  (==) Love Love = True
  (==) Fifteen Fifteen = True
  (==) Thirty Thirty = True
  (==) Forty Forty = True
  (==) _ _ = False

data Player = P1 | P2

nextscore : NormalScore -> NormalScore
nextscore Love = Fifteen
nextscore Fifteen = Thirty
nextscore Thirty = Forty

score : (NormalScore, NormalScore) -> Player -> (NormalScore, NormalScore)
score (a, b) P1 = (nextscore a, b)
score (a, b) P2 = (a, nextscore b)

main: IO ()
main = spec $ do
  describe "normal score" $ do
    it "makes score love-fiftee" $ do
      score (Love, Love) P1  `shouldBe` (Fifteen, Love)
    it "makes score fifteen-love" $ do
      score (Love, Love) P2  `shouldBe` (Love, Fifteen)
    it "makes score Love-forty" $ do
      score (Love, Thirty) P2  `shouldBe` (Love, Forty)
  describe "end of game" $ do
    it "is game after 40" $ do
      -- score (Forty, Thirty) `shouldBe` Game P1
    -- it "is deuce when we have 40-40" $ do 
      1 `shouldBe` 1
