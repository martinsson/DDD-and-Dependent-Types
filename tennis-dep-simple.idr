data Player = P1 | P2
data PlayerScore = Love | Fifteen | Thirty | Forty

data PointScore PlayerScore PlayerScore = MkPointScore p1Score p2Score

mutual 
  data Deuce : Type where 
    DeuceFromPoints : PointScore p1Score p2Score -> Deuce
    DeuceFromAdvantage : Advantage player -> Deuce
  
  data Advantage : Player -> Type where
    MkAdvantage: Deuce -> (player: Player) -> Advantage player
  
  data Win : Player -> Type where
    MkWin: Advantage player -> Win player


