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
tennisPoint 3 = "forty"
tennisPoint _ = "impossible!!"

Show Score where
  show (MkScore p1Score p2Score) = let diff = p1Score - p2Score 
                                       isInPlayoff = p1Score >= 3 && p2Score >= 3 in
    if isInPlayoff then 
      case diff + 2 of  -- bug? in parser cant match on negative values 
           4 => "game player1"
           3 => "advantage player1"
           2 => "deuce"
           1 => "advantage player2"
           0 => "game player2"
           _ => "impossible!!"
    else
      case (p1Score, p2Score, diff >= 2, diff <= -2) of
           (4, _, True, _) => "game player1"
           (_, 4, _, True) => "game player2"
           (p1, p2, _, _) =>  tennisPoint p1 ++ "-" ++ tennisPoint p2

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
		it "handles deuce from advantage" $ 
			score [P1, P1, P1, P2, P2, P2, P2, P1] `shouldBe` "deuce"
  describe "advantage" $ do
    it "handles P1" $
			score [P1, P1, P1, P2, P2, P2, P1] `shouldBe` "advantage player1"
    it "handles P2" $
			score [P1, P1, P1, P2, P2, P2, P2] `shouldBe` "advantage player2"
    it "handles long games" $
			score [P1, P1, P1, P2, P2, P2, P1, P2, P1, P2, P1] `shouldBe` "advantage player1"
  describe "win" $ do
    it "handles game ball P1" $
      score [P1, P1, P1, P1] `shouldBe` "game player1"
    it "handles game ball P2" $
      score [P2, P2, P2, P2] `shouldBe` "game player2"
    it "handles game ball from advantage" $
      score [P1, P1, P1, P2, P2, P2, P1, P1] `shouldBe` "game player1"
    it "handles game ball P2" $
      score [P1, P1, P1, P2, P2, P2, P1, P2, P2, P2] `shouldBe` "game player2"
 
