module Pair where

instance (Num a, Num b) => Num (a,b) where
  (a1,b1) + (a2,b2) = (a1+a2, b1+b2)
  (a1,b1) * (a2,b2) = (a1*a2, b1*b2)
  abs    (a,b)  = (abs    a, abs    b)
  signum (a,b)  = (signum a, signum b)
  negate (a,b)  = (negate a, negate b)
  fromInteger x = (fromInteger x, fromInteger x)  

instance (Fractional a, Fractional b) => Fractional (a,b) where
  fromRational x = (fromRational x, fromRational x)
  (a1,b1) / (a2,b2) = (a1/a2, b1/b2)

dup :: a -> (a,a)
dup x = (x,x)

