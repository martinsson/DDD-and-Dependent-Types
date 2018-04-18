data Player = P1 | P2
data PlayerPoints = Love | Fifteen | Thirty | Forty

data PointScore PlayerPoints PlayerPoints = MkPointScore p1Points p2Points

mutual
  
  data Deuce : Type where 
    DeuceFromPoints : PointScore p1Points p2Points -> Deuce
    DeuceFromAdvantage : Advantage player -> Deuce
  
  data Advantage : Player -> Type where
    MkAdvantage: Deuce -> (player: Player) -> Advantage player
  
  data Win : Player -> Type where
    MkWin: Advantage player -> Win player

data Score = WrapPointScore (PointScore PlayerPoints PlayerPoints) | 
             WrapDeuce Deuce |
             WrapAdvantage (Advantage player) |
             WrapWin (Win player)

score : (ballWins: List Player) -> Score
score ballWins = let initialScore = (WrapPointScore (MkPointScore Love Love)) in 
                     scoreHelper initialScore ballWins where 
 
  nextScore : Score -> Player -> Score
  nextScore (WrapPointScore currentScore) ballWinner = ?nextScore_rhs_1
  nextScore (WrapDeuce currentScore) ballWinner = WrapAdvantage (MkAdvantage currentScore ballWinner)
  nextScore (WrapAdvantage currentScore) ballWinner = ?nextScore_rhs_3
  nextScore (WrapWin y) ballWinner = WrapWin y
  
  scoreHelper : (currentScore: Score) -> List Player -> Score
  scoreHelper currentScore [] = currentScore
  scoreHelper currentScore (y :: xs) = scoreHelper (nextScore currentScore y) xs
