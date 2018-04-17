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


