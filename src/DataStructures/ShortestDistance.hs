module DataStructures.ShortestDistance where 

import Algebra.Semiring


-----------------------------------------------------------------------------------
---------------                    DEFINITION                       ---------------
-----------------------------------------------------------------------------------
data ShortestDistance a = Distance a | Unreachable deriving Show
-----------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
---------------               TYPECLASSES' INSTANCES                ---------------
-----------------------------------------------------------------------------------
-- Semiring
instance (Semiring a, Ord a) => Semiring (ShortestDistance a) where 
  zero = Unreachable 
  one  = Distance zero
  
  x <+> Unreachable = x
  Unreachable <+> x = x 
  Distance a <+> Distance b = Distance (min a b)

  x <.> Unreachable = Unreachable 
  Unreachable <.> x = Unreachable 
  Distance a <.> Distance b = Distance (a <+> b)

-- StarSemiring
instance (Semiring a, Ord a) => StarSemiring (ShortestDistance a) where 
  closure _ = one 

-- Eq
instance Eq a => Eq (ShortestDistance a) where 
  Unreachable == Unreachable = True 
  Distance d1 == Distance d2 = d1 == d2
  _ == _ = False 

-- Ord
instance Ord a => Ord (ShortestDistance a) where 
  Unreachable <= Unreachable = True 
  Distance d1 <= Distance d2 = d1 <= d2 
  Distance _  <= Unreachable = True 
  _ <= _ = False  
-----------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
---------------                  UTILITY FUNCTIONS                  ---------------
-----------------------------------------------------------------------------------
fromDistance :: ShortestDistance a -> a 
fromDistance (Distance d) = d
fromDistance Unreachable  = error "fromDistance: Expected (Distance d) but got Unreachable."
-----------------------------------------------------------------------------------
