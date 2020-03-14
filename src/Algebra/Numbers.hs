{-# LANGUAGE FlexibleInstances #-}

-- Semiring and Star-Semiring instances of numerical types
-- Useful for defining different types of weights in graphs
-- See, e.g., DataStructures.ShortestDistance


module Algebra.Numbers where 

import Algebra.Semiring
import Data.Ratio


-- Int
instance Semiring Int where 
  one   = 1
  zero  = 0
  (<+>) = (+)
  (<.>) = (*)


-- Integer
instance Semiring Integer where 
  one   = 1
  zero  = 0
  (<+>) = (+)
  (<.>) = (*)


-- Rational
instance Semiring Rational where 
  one   = 1 % 1
  zero  = 0 % 1
  (<+>) = (+)
  (<.>) = (*)

instance StarSemiring Rational where
  closure x = (1 % 1) / (1 % 1 - x)


-- Double
instance Semiring Double where 
  one   = 1.0
  zero  = 0.0
  (<+>) = (+)
  (<.>) = (*)

instance StarSemiring Double where
  closure x = 1.0 / (1.0 - x)


-- Float
instance Semiring Float where 
  one   = 1.0
  zero  = 0.0
  (<+>) = (+)
  (<.>) = (*)

instance StarSemiring Float where
  closure x = 1.0 / (1.0 - x)