module Algebra.Semiring where

infixl 9 <.>
infixl 8 <+>

class Semiring a where
  one  :: a
  zero :: a

  (<+>) :: a -> a -> a
  (<.>) :: a -> a -> a


class Semiring a => StarSemiring a where
  closure :: a -> a