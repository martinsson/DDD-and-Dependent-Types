%default total

data Player = P1 | P2
            

data Deuce = MkDeuce

data Advantage = MkAdvantage Deuce

data Game = MkGame Advantage

data TennisScore = ScoreDeuce Deuce | ScoreAdvantage Advantage | ScoreGame Game

-- TennisScoreFn : score -> Player -> Type
-- TennisScoreFn Deuce P1 = Advantage 

  
interface Score s where
  score : s -> (p: Player) -> TennisScore
--   score : s -> (p: Player) -> TennisScoreFn s p

Score Deuce where
  score s p = ScoreAdvantage $ MkAdvantage s

Score Advantage where
  score s p = ScoreGame $ MkGame s

play : TennisScore -> Player -> TennisScore
play (ScoreDeuce s) p = score s p
play (ScoreAdvantage s) p = score s p
play (ScoreGame s) p = ScoreGame s
