module DataStructures.ShortestDistance where 

import Algebra.Semiring


data ShortestDistance = Distance Int | Unreachable deriving Show

instance Semiring ShortestDistance where 
  zero = Unreachable 
  one = Distance 0 
  
  x <+> Unreachable = x
  Unreachable <+> x = x 
  Distance a <+> Distance b = Distance (min a b)

  x <.> Unreachable = Unreachable 
  Unreachable <.> x = Unreachable 
  Distance a <.> Distance b = Distance (a + b)

instance StarSemiring ShortestDistance where 
  closure _ = one 


instance Eq ShortestDistance where 
  Unreachable == Unreachable = True 
  Distance d1 == Distance d2 = True 
  _ == _ = False 

instance Ord ShortestDistance where 
  Unreachable <= Unreachable = True 
  Distance d1 <= Distance d2 = d1 <= d2 
  Distance _  <= Unreachable = True 
  _ <= _ = False  