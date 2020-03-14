module DataStructures.ShortestPath where 

import Algebra.Numbers
import Algebra.Semiring


data ShortestPath d n = Path d [(n,n)] | NoPath deriving (Show)


instance (Semiring d, Ord d, Ord n) => Semiring (ShortestPath d n) where
  zero = NoPath
  one  = Path zero []

  x <+> NoPath = x
  NoPath <+> x = x
  Path a p <+> Path a' p'
    | a < a'            = Path a p
    | a == a' && p < p' = Path a p
    | otherwise         = Path a' p'
  
  x <.> NoPath = NoPath
  NoPath <.> x = NoPath
  Path a p <.> Path a' p' = Path (a <+> a') (p ++ p')


instance (Semiring d, Ord d, Ord n) => StarSemiring (ShortestPath d n) where 
  closure _ = one 

