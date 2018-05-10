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

||| I wonder why we need to use the full form of type definition and why the
||| simpler form doesn't work i.e.
||| data PointScore PlayerPoints PlayerPoints = MkPointScore p1Points p2Points
data PointScore : PlayerPoints -> PlayerPoints -> Type where
     MkPointScore: (p1Points: PlayerPoints) -> (p2Points: PlayerPoints) ->
                   PointScore p1Points p2Points

||| This type, representing the fact that one player has reached Forty allows to
||| reduce cardinality (=~ cyclomatic complexity) of the nextScore on PointScore
||| as in this case we have only two possibilities: FortyOf player or another
||| PointScore. Without this type we'd have three possibilites : Deuce, Win
||| or another PointScore
data FortyOf : Player -> Type where
    MkFortyP1 : (PointScore Thirty p2points) -> FortyOf P1
    MkFortyP2 : (PointScore p1Points Thirty) -> FortyOf P2

mutual
  data Deuce = DeuceFromForty (FortyOf player) | 
               DeuceFromAdvantage (Advantage player)
  
  data Advantage : Player -> Type where
    MkAdvantage: Deuce -> (player: Player) -> Advantage player
  
  data Win : Player -> Type where
    WinFromAdvantage: Advantage player -> Win player
    WinFromForty: FortyOf player -> Win player

||| All types are disjoint, so we need a Union type in order to define a
||| function on any type of score.
||| BTW I find this syntax for an Union type really horrible and clunky
data Score = WrapPointScore (PointScore p1Points p2Points) | 
             WrapForty (FortyOf player) | 
             WrapDeuce Deuce |
             WrapAdvantage (Advantage player) |
             WrapWin (Win player)

||| type-class or polymorphic function that calculates the next score based on
||| the current and the player. Could be implemented also for Win and Deuce but
||| their implementation is trivial so it is more concise to inline the code in
||| the function applyNextScore below
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
                     scoreHelper ballWins initialScore where 
 
  applyNextScore : Player -> Score -> Score
  applyNextScore ballWinner (WrapPointScore currentScore) = nextScore currentScore ballWinner
  applyNextScore ballWinner (WrapForty currentScore)      = nextScore currentScore ballWinner
  applyNextScore ballWinner (WrapDeuce currentScore)      = WrapAdvantage (MkAdvantage currentScore ballWinner)
  applyNextScore ballWinner (WrapAdvantage currentScore)  = nextScore currentScore ballWinner
  applyNextScore ballWinner (WrapWin currentScore)        = WrapWin currentScore
  
  -- recursive version of the score function
  scoreHelper : (ballWinners: List Player) -> Score -> Score
  scoreHelper [] score                          = score
  scoreHelper (ballWinner :: ballWinners) score = scoreHelper ballWinners $ applyNextScore ballWinner score





