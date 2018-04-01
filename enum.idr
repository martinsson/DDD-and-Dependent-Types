data Players = P1 | P2
%default total

Enum Players where 
  pred x = fromNat (S (toNat x))

  toNat P1 = 0
  toNat P2 = 1

  fromNat Z = P1
  fromNat (S (S k)) = fromNat k
  fromNat (S k) = P2
