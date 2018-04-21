data Player = P1 | P2
data PlayerPoints = Love | Fifteen | Thirty | Forty

data PointScore PlayerPoints PlayerPoints = MkPointScore PlayerPoints PlayerPoints

mutual
  data Deuce : Type where 
    DeuceFromPoints : PointScore p1Points p2Points -> Deuce
    DeuceFromAdvantage : Advantage player -> Deuce
  
  data Advantage : Player -> Type where
    MkAdvantage: Deuce -> (player: Player) -> Advantage player
  
  data Win : Player -> Type where
    WinFromAdvantage: Advantage player -> Win player
    -- TODO make this stricter
    WinFromForty: PointScore PlayerPoints PlayerPoints -> (player: Player) -> Win player

data Score = WrapPointScore (PointScore PlayerPoints PlayerPoints) | 
             WrapDeuce Deuce |
             WrapAdvantage (Advantage player) |
             WrapWin (Win player)

Eq Player where
  (==) P1 P1 = True
  (==) P2 P2 = True
  (==) _  _  = False 

interface NextScore currentScore where
  nextScore : currentScore -> Player -> Score

NextScore (Advantage playerWithAdvantage) where
  nextScore advantage@(MkAdvantage _ playerWithAdvantage) winnerOfBall = 
    if playerWithAdvantage == winnerOfBall 
       then WrapWin (WinFromAdvantage advantage)
       else WrapDeuce (DeuceFromAdvantage advantage)
  
nextPoint : PlayerPoints -> PlayerPoints 
nextPoint Love = Fifteen
nextPoint Fifteen = Thirty
nextPoint Thirty = Forty
nextPoint Forty = Forty -- argh!

NextScore (PointScore PlayerPoints PlayerPoints) where
  nextScore (MkPointScore p1Points p2Points) ballWinner = nextPointScore p1Points p2Points ballWinner where

    nextPointScore : (p1Points : PlayerPoints) -> (p2Points : PlayerPoints) -> (ballWinner : Player) -> Score
    nextPointScore Thirty Forty P1 = WrapDeuce (DeuceFromPoints (MkPointScore p1Points p2Points))
    nextPointScore Forty Thirty P2 = WrapDeuce (DeuceFromPoints (MkPointScore p1Points p2Points))
    nextPointScore Forty _ P1 = WrapWin (WinFromForty (MkPointScore p1Points p2Points) P1)
    nextPointScore _ Forty P2 = WrapWin (WinFromForty (MkPointScore p1Points p2Points) P2)
    nextPointScore p1Points p2Points P1 = WrapPointScore (MkPointScore (nextPoint p1Points) p2Points)
    nextPointScore p1Points p2Points P2 = WrapPointScore (MkPointScore p1Points (nextPoint p2Points))
  

score : (ballWins: List Player) -> Score
score ballWins = let initialScore = (WrapPointScore (MkPointScore Love Love)) in 
                     scoreHelper initialScore ballWins where 
 
  applyScore : Score -> Player -> Score
  applyScore (WrapPointScore currentScore) ballWinner = nextScore currentScore ballWinner
  applyScore (WrapDeuce currentScore) ballWinner = WrapAdvantage (MkAdvantage currentScore ballWinner)
  applyScore (WrapAdvantage advantage) ballWinner = nextScore advantage ballWinner
  applyScore (WrapWin y) ballWinner = WrapWin y
  
  scoreHelper : (currentScore: Score) -> List Player -> Score
  scoreHelper currentScore [] = currentScore
  scoreHelper currentScore (y :: xs) = scoreHelper (applyScore currentScore y) xs
