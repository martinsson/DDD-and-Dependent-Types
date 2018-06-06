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
Show PlayerPoints where
  show Love = "0"
  show Fifteen = "15"
  show Thirty = "30"

nextPoint : (playerPoint: PlayerPoints) -> PlayerPoints -- -> {prf: (playerPoint = Thirty) 
nextPoint Love    = Fifteen
nextPoint Fifteen = Thirty
nextPoint Thirty  = Thirty -- TODO impossible

||| I wonder why we need to use the full form of type definition and why the
||| simpler form doesn't work i.e.
||| data PointScore PlayerPoints PlayerPoints = MkPointScore p1Points p2Points
data PointScore : PlayerPoints -> PlayerPoints -> Type where
     MkPointScore: (p1Points: PlayerPoints) -> (p2Points: PlayerPoints) ->
                   PointScore p1Points p2Points

||| This type, representing the fact that one player has reached Forty allows to
||| reduce cardinality (=~ cyclomatic complexity) of the implementation of nextScore
||| for PointScore below. PointScore already has to deal with the different 
||| PlayerPoints.  
data GameBall : Player -> PlayerPoints -> Type where
  FromThirtyP1 : (PointScore Thirty p2Points) -> GameBall P1 p2Points
  FromThirtyP2 : (PointScore p1Points Thirty) -> GameBall P2 p1Points
  FromGameBall : (GameBall player otherPlayerPoints) -> -- here use the proof of not thirty
                 GameBall player (nextPoint otherPlayerPoints)

mutual
  data Deuce = DeuceFromGameBall (GameBall player op) | 
               DeuceFromAdvantage (Advantage player)
  
  data Advantage : Player -> Type where
    MkAdvantage: Deuce -> (player: Player) -> Advantage player
  
  data Win : Player -> Type where
    WinFromAdvantage: Advantage player -> Win player
    WinFromGameBall: GameBall player _ -> Win player

||| All types are disjoint, so we need a Union type in order to define a
||| function on any type of score.
||| BTW I'm curious as to why we can't define score as a type alias, like an 
||| OR-type find wrapping and unwrapping of the Union type really cumbersome
data Score = WrapPointScore (PointScore p1Points p2Points) | 
             WrapGameBall (GameBall player otherPlayerPoints) | 
             WrapDeuce Deuce |
             WrapAdvantage (Advantage player) |
             WrapWin (Win player)

Show Score where 
  show (WrapPointScore (MkPointScore p1Points p2Points)) = show p1Points ++ " - " ++ show p2Points
  show (WrapGameBall x) = ?Show_rhs_3
  show (WrapDeuce x) = "deuce"
  show (WrapAdvantage x) = ?Show_rhs_5
  show (WrapWin x) = ?Show_rhs_6

||| type-class or polymorphic function that calculates the next score based on
||| the current and the player that won the ball. Could be implemented also for
||| Win and Deuce but their implementation is trivial so it is more concise to
||| inline the code in the function applyNextScore below
interface NextScore currentScore where
  nextScore : currentScore -> Player -> Score

NextScore (GameBall player playerPoints) where
  nextScore currentScore P1 {player = P1} = WrapWin (WinFromGameBall currentScore)
  nextScore currentScore P2 {player = P2} = WrapWin (WinFromGameBall currentScore)
  nextScore currentScore ballWinner {playerPoints = Thirty} = WrapDeuce (DeuceFromGameBall currentScore) 
  nextScore currentScore ballWinner {playerPoints = _} = WrapGameBall (FromGameBall currentScore) 

NextScore (Advantage player) where
  nextScore currentScore P1 {player = P1} = WrapWin (WinFromAdvantage currentScore)
  nextScore currentScore P2 {player = P2} = WrapWin (WinFromAdvantage currentScore)
  nextScore currentScore _                = WrapDeuce (DeuceFromAdvantage currentScore)
  
NextScore (PointScore p1Points p2Points) where
  nextScore currentScore P1 {p1Points = Thirty}   = WrapGameBall (FromThirtyP1 currentScore)
  nextScore currentScore P2 {p2Points = Thirty}   = WrapGameBall (FromThirtyP2 currentScore)
  nextScore currentScore P1 {p1Points} {p2Points} = WrapPointScore (MkPointScore (nextPoint p1Points) p2Points)
  nextScore currentScore P2 {p1Points} {p2Points} = WrapPointScore (MkPointScore p1Points (nextPoint p2Points))
  
-- recursive version of the score function
scoreHelper : (ballWinners: List Player) -> Score -> Score
scoreHelper [] score                          = score
scoreHelper (ballWinner :: ballWinners) score = scoreHelper ballWinners $ applyNextScore ballWinner score where

  applyNextScore : Player -> Score -> Score
  applyNextScore ballWinner (WrapPointScore currentScore) = nextScore currentScore ballWinner
  applyNextScore ballWinner (WrapGameBall currentScore )  = nextScore currentScore ballWinner
  applyNextScore ballWinner (WrapDeuce currentScore)      = WrapAdvantage (MkAdvantage currentScore ballWinner)
  applyNextScore ballWinner (WrapAdvantage currentScore)  = nextScore currentScore ballWinner
  applyNextScore ballWinner (WrapWin currentScore)        = WrapWin currentScore

score : (ballWins: List Player) -> Score
score ballWins = let initialScore = (WrapPointScore (MkPointScore Love Love)) in 
                     scoreHelper ballWins initialScore 
 
showScore : List Player -> String
showScore xs = show $ score xs

-- Tests ---

aSimpleScore : showScore [P1, P2, P2] = "15 - 30"
aSimpleScore = Refl

partial
deuceIsEqualityAfter3PointsEach1 : (pointsEach: Nat ) -> showScore (join (replicate (3 + pointsEach)  [P1, P2] )) = "deuce" 
deuceIsEqualityAfter3PointsEach1 Z = Refl
deuceIsEqualityAfter3PointsEach1 (S Z) = Refl
deuceIsEqualityAfter3PointsEach1 (S (S Z)) = Refl
deuceIsEqualityAfter3PointsEach1 (S (S (S Z))) = Refl
deuceIsEqualityAfter3PointsEach1 (S (S (S (S Z)))) = Refl

partial
deuceIsEqualityAfter3PointsEach : (pointsEach: Nat ) -> (prf: LTE pointsEach 2) -> showScore (join (replicate (3 + pointsEach)  [P1, P2] )) = "deuce" 
deuceIsEqualityAfter3PointsEach Z prf = Refl
deuceIsEqualityAfter3PointsEach (S k) _ with (compare (S k) k) proof prf
  deuceIsEqualityAfter3PointsEach (S k) _ | EQ = ?slkjdf

partial
deuceIsEqualityAfter3PointsEach2 : (pointsEach: Nat ) -> (prf: LTE pointsEach 2) -> showScore (join (replicate (3 + pointsEach)  [P1, P2] )) = "deuce" 
deuceIsEqualityAfter3PointsEach2 Z prf = Refl
deuceIsEqualityAfter3PointsEach2 (S Z) prf = Refl
deuceIsEqualityAfter3PointsEach2 (S (S Z)) prf = Refl
deuceIsEqualityAfter3PointsEach2 (S (S (S _))) (LTESucc (LTESucc LTEZero)) impossible
deuceIsEqualityAfter3PointsEach2 (S (S (S _))) (LTESucc (LTESucc (LTESucc _))) impossible
-- deuceIsEqualityAfter3PointsEach2 k prf = Refl

-- trying to proov that we go back and forth to deuce
deuceAdvantageDeuce : (score: Score) -> {prf: score = (WrapDeuce d)} -> scoreHelper [P1, P2] score = WrapDeuce (DeuceFromAdvantage (MkAdvantage d P1))
deuceAdvantageDeuce score = ?deuceAdvantageDeuce_rhs


