import Specdris.Spec

%default total
data Player = P1 | P2

data Score = MkScore Integer Integer 

scoreValue : List Player -> Score 
scoreValue ballWinners = foldl nextScoreValue (MkScore 0 0) ballWinners where
  nextScoreValue : Score -> (ballWinner: Player) -> Score 
  nextScoreValue (MkScore p1Score p2Score) P1 = MkScore (p1Score + 1) p2Score
  nextScoreValue (MkScore p1Score p2Score) P2 = MkScore p1Score (p2Score + 1)

tennisPoint : Integer -> String
tennisPoint 0 = "love"
tennisPoint 1 = "fifteen"
tennisPoint 2 = "thirty"
tennisPoint _ = "forty"

Show Score where
  show (MkScore p1Score p2Score) = let diff = p1Score - p2Score in
    case (p1Score, p2Score, diff) of
         (3, 3, _) => "deuce"
         (p1, p2, _) =>  tennisPoint p1 ++ "-" ++ tennisPoint p2

score : List Player -> String
score ballWinners = show $ scoreValue ballWinners

main: IO ()
main = spec $ do
  describe "normal score" $ do
    it "makes score love-love" $ 
      score [] `shouldBe` "love-love"
    it "makes score with points" $ 
      score [P1, P2, P2] `shouldBe` "fifteen-thirty"
    it "handles forty" $ 
      score [P2, P1, P1, P2, P2] `shouldBe` "thirty-forty"
  describe "deuce" $ do
		it "handles deuce from points" $ 
			score [P1, P1, P1, P2, P2, P2] `shouldBe` "deuce"
 
