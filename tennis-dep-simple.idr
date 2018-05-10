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

nextPoint : PlayerPoints -> PlayerPoints 
nextPoint Love    = Fifteen
nextPoint Fifteen = Thirty
nextPoint Thirty  = Thirty

data PointScore : PlayerPoints -> PlayerPoints -> Type where
     MkPointScore: (p1Points: PlayerPoints) -> (p2Points: PlayerPoints) ->
                   PointScore p1Points p2Points

-- data PointScore PlayerPoints PlayerPoints = MkPointScore p1Points p2Points

data FortyOf : Player -> Type where
    MkFortyP1 : (PointScore Thirty p2points) -> FortyOf P1
    MkFortyP2 : (PointScore p1Points Thirty) -> FortyOf P2

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
data Score = WrapPointScore (PointScore p1Points p2Points) | 
             WrapForty (FortyOf player) | 
             WrapDeuce Deuce |
             WrapAdvantage (Advantage player) |
             WrapWin (Win player)

interface NextScore currentScore where
  nextScore : currentScore -> Player -> Score

NextScore (FortyOf player) where
  nextScore currentScore P1 {player = P1} = WrapWin (WinFromForty currentScore)
  nextScore currentScore P2 {player = P2} = WrapWin (WinFromForty currentScore)
  nextScore currentScore _                = WrapDeuce (DeuceFromForty currentScore)

NextScore (Advantage player) where
  nextScore currentScore P1 {player = P1} = WrapWin (WinFromAdvantage currentScore)
  nextScore currentScore P2 {player = P2} = WrapWin (WinFromAdvantage currentScore)
  nextScore currentScore _                = WrapDeuce (DeuceFromAdvantage currentScore)
  
NextScore (PointScore p1Points p2Points) where
  nextScore currentScore@(MkPointScore Thirty p2Points) ballWinner {p1Points = Thirty} = WrapForty (MkFortyP1 currentScore)
  nextScore currentScore@(MkPointScore p1Points Thirty) ballWinner {p2Points = Thirty} = WrapForty (MkFortyP2 currentScore)
  nextScore currentScore P1 {p1Points} {p2Points} = WrapPointScore (MkPointScore (nextPoint p1Points) p2Points)
  nextScore currentScore P2 {p1Points} {p2Points} = WrapPointScore (MkPointScore p1Points (nextPoint p2Points))

score : (ballWins: List Player) -> Score
score ballWins = let initialScore = (WrapPointScore (MkPointScore Love Love)) in 
                     scoreHelper initialScore ballWins where 
 
  applyNextScore : Score -> Player -> Score
  applyNextScore (WrapPointScore currentScore) ballWinner = nextScore currentScore ballWinner
  applyNextScore (WrapForty currentScore) ballWinner      = nextScore currentScore ballWinner
  applyNextScore (WrapDeuce currentScore) ballWinner      = WrapAdvantage (MkAdvantage currentScore ballWinner)
  applyNextScore (WrapAdvantage currentScore) ballWinner  = nextScore currentScore ballWinner
  applyNextScore (WrapWin currentScore) _                 = WrapWin currentScore
  
  scoreHelper : Score -> List Player -> Score
  scoreHelper score []                          = score
  scoreHelper score (ballWinner :: ballWinners) = scoreHelper (applyNextScore score ballWinner) ballWinners





