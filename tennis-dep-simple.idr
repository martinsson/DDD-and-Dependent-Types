%default total 
-- The types expresses the state-pattern of Tennis, the Type constructors take the
-- previous score as an argument hence disallowing illegal state transitions
-- some transitions use dependent types to enforce that it is only the player
-- who had the advantage that can win after the next ball.

-- All types are disjoint, i.e. instead of making one type GameScore and many type constructors
-- which feels like inheritance. Here we use composition of types, this strategy seems more extensible
-- (not that tennis is ever going to be extended...) 

data Player = P1 | P2
Eq Player where
  (==) P1 P1 = True
  (==) P2 P2 = True
  (==) _  _  = False 

data PlayerPoints = Love | Fifteen | Thirty 

data PointScore = MkPointScore PlayerPoints PlayerPoints

data FortyOf : Player -> Type where
    MkForty : (player: Player) -> FortyOf player

mutual
  data Deuce : Type where 
    DeuceFromForty : FortyOf player -> Deuce
    DeuceFromAdvantage : Advantage player -> Deuce
  
  data Advantage : Player -> Type where
    MkAdvantage: Deuce -> (player: Player) -> Advantage player
  
  data Win : Player -> Type where
    WinFromAdvantage: Advantage player -> Win player
    WinFromForty: FortyOf player -> Win player

-- I find this syntax for an Union type really horrible and clunky
data Score = WrapPointScore (PointScore) | 
						 WrapForty (FortyOf player) | 
             WrapDeuce Deuce |
             WrapAdvantage (Advantage player) |
             WrapWin (Win player)

interface NextScore currentScore where
  nextScore : currentScore -> Player -> Score

NextScore (FortyOf player) where
  nextScore currentScore@(MkForty player) ballWinner = 
		if player == ballWinner 
			then WrapWin (WinFromForty currentScore)
			else WrapDeuce (DeuceFromForty currentScore) 

NextScore (Advantage player) where
  nextScore currentScore@(MkAdvantage _ player) winnerOfBall = 
    if player == winnerOfBall 
       then WrapWin (WinFromAdvantage currentScore)
       else WrapDeuce (DeuceFromAdvantage currentScore)
  
NextScore (PointScore) where
  nextScore (MkPointScore p1Points p2Points) ballWinner = nextPointScore p1Points p2Points ballWinner where

    nextPoint : PlayerPoints -> PlayerPoints 
    nextPoint Love    = Fifteen
    nextPoint Fifteen = Thirty
    nextPoint Thirty  = Thirty

    nextPointScore : (p1Points : PlayerPoints) -> (p2Points : PlayerPoints) -> (ballWinner : Player) -> Score
    nextPointScore   Thirty       _  P1 = WrapForty (MkForty P1) 
    nextPointScore        _   Thirty P2 = WrapForty (MkForty P2) 
    nextPointScore p1Points p2Points P1 = WrapPointScore (MkPointScore (nextPoint p1Points) p2Points)
    nextPointScore p1Points p2Points P2 = WrapPointScore (MkPointScore p1Points (nextPoint p2Points))
  

score : (ballWins: List Player) -> Score
score ballWins = let initialScore = (WrapPointScore (MkPointScore Love Love)) in 
                     scoreHelper initialScore ballWins where 
 
  applyScore : Score -> Player -> Score
  applyScore (WrapPointScore currentScore) ballWinner = nextScore currentScore ballWinner
  applyScore (WrapForty currentScore) ballWinner      = nextScore currentScore ballWinner
  applyScore (WrapDeuce currentScore) ballWinner      = WrapAdvantage (MkAdvantage currentScore ballWinner)
  applyScore (WrapAdvantage currentScore) ballWinner  = nextScore currentScore ballWinner
  applyScore (WrapWin y) ballWinner 									= WrapWin y
  
  scoreHelper : (currentScore: Score) -> List Player -> Score
  scoreHelper currentScore []        = currentScore
  scoreHelper currentScore (y :: xs) = scoreHelper (applyScore currentScore y) xs





