module Noise
  ( module Hash
  , noise ) where

import Data.Ratio

import Hash

class Noise a where
  noise :: Hashable b => b -> a

instance Noise Float where
  noise x = fromIntegral h / fromIntegral (m-1)
    where
      h = hash x `mod` m
      m = 2^16 

instance (Integral a) => Noise (Ratio a) where
  noise x = (h - mn) / (mx - mn)
    where
      h  = fromIntegral (hash x)
      mn = fromIntegral (minBound :: Hash)
      mx = fromIntegral (maxBound :: Hash)

instance (Noise a, Noise b) => Noise (a,b) where
  noise x = (noise h, noise (h+1))
    where h = hash x
