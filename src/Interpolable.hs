module Interpolable where

class Interpolable a where
  lerp :: Real w => a -> a -> w -> a
  
instance Interpolable Float where
  lerp a b w = a * (1 - realToFrac w) + b * realToFrac w
