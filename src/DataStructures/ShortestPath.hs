module DataStructures.ShortestPath where 

import Algebra.Semiring


data ShortestPath n = Path Int [(n,n)] | NoPath deriving (Show)


instance Ord n => Semiring (ShortestPath n) where
  zero = NoPath
  one  = Path 0 []

  x <+> NoPath = x
  NoPath <+> x = x
  Path a p <+> Path a' p'
    | a < a'            = Path a p
    | a == a' && p < p' = Path a p
    | otherwise         = Path a' p'
  
  x <.> NoPath = NoPath
  NoPath <.> x = NoPath
  Path a p <.> Path a' p' = Path (a + a') (p ++ p')


instance Ord n => StarSemiring (ShortestPath n) where 
  closure _ = one 

