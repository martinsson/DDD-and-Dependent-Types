%default total


||| interesting type function in a type class
||| we're only concernet with the next state in each implementation of Score
||| type logic working like double entry book keeping/tdd
||| thinking about the next type/state really clarified the problem and helped reach a solution (tom)

data Player = P1 | P2
            

data Deuce = MkDeuce

data Advantage Player = MkAdvantage Deuce Player

data Game = MkGame (Advantage player)

-- TennisScore = ScoreDeuce Deuce | ScoreAdvantage Advantage | ScoreGame Game

-- TennisScoreFn : score -> Player -> Type
-- TennisScoreFn Deuce P1 = Advantage 
-- data Score score = MkScore score
-- DScore : DPair a (Score a)
  
interface Score s where
  Next : s -> (p: Player) -> Type
  score : (lastScore : s) -> (p: Player) -> Next lastScore p
--   score : s -> (p: Player) -> TennisScoreFn s p

Score Deuce where
  Next Deuce player = Advantage Player

  score s p = MkAdvantage s p

Score (Advantage player) where
  Next (MkAdvantage _ P1) P1 = Game
  Next (MkAdvantage _ P2) P2 = Game
  Next (MkAdvantage _ _) _ = Deuce

  score s@(MkAdvantage x P1) P1 = MkGame s
  score s@(MkAdvantage x P2) P2 = MkGame s
  score s@(MkAdvantage x P1) P2 = x
  score s@(MkAdvantage x P2) P1 = x

Score Game where
  Next Game = Void
  score s p = absurd


play : (Score p) -> Player -> (Score n)
play prevScore player = ?helpme

-- play : TennisScore -> Player -> TennisScore
-- play (ScoreDeuce s) p = score s p
-- play (ScoreAdvantage s) p = score s p
-- play (ScoreGame s) p = ScoreGame s
