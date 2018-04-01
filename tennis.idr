import Specdris.Spec
%default total

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
Eq Player where
  (==) P1 P1 = True
  (==) P2 P2 = True
  (==) _ _ = False
Show Player where
  show P1 = "P1"
  show P2 = "P2"

data GameScore : Type where
  NormalGameScore : (NormalScore, NormalScore) -> GameScore
  Deuce           : GameScore
  Advantage       : Player -> GameScore
  Game            : Player -> GameScore

Show GameScore where
  show (NormalGameScore normalScoreTuple) = show normalScoreTuple
  show (Game player) = "Game " ++ show player
  show Deuce = "Deuce"
  show (Advantage player) = "Advantage " ++ show player

Eq GameScore where
  (==) (NormalGameScore x) (NormalGameScore y) = x == y
  (==) (Game x) (Game y) = x == y
  (==) (Deuce) (Deuce) = True
  (==) (Advantage px) (Advantage py) = px == py
  (==) _ _ = False

nextscore : NormalScore -> NormalScore
nextscore Love = Fifteen
nextscore Fifteen = Thirty
nextscore Thirty = Forty
nextscore Forty = Forty

score : (NormalScore, NormalScore) -> Player -> (NormalScore, NormalScore)
score (a, b) P1 = (nextscore a, b)
score (a, b) P2 = (a, nextscore b)

score2 : GameScore -> Player -> GameScore
score2 (NormalGameScore (Forty, Thirty)) P2 = Deuce
score2 (NormalGameScore (Thirty, Forty)) P1 = Deuce
score2 (NormalGameScore (Forty, b)) P1 = Game P1
score2 (NormalGameScore (a, Forty)) P2 = Game P2
score2 (NormalGameScore normalScore) player = NormalGameScore $ score normalScore player
score2 (Deuce) p = Advantage p
score2 (Advantage px) py = if px == py then Game px else Deuce
score2 (Game x) y = Game x

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
      score2 (NormalGameScore (Forty, Thirty)) P1 `shouldBe` Game P1
    it "is deuce when we have 40-40" $ do 
      score2 (NormalGameScore (Forty, Thirty)) P2 `shouldBe` Deuce
    it "is advantage after deuce" $ do 
      score2 Deuce P1 `shouldBe` Advantage P1
