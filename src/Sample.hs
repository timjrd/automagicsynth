{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sample where

import Data.Int
import Data.Bits
import Data.Fixed.Binary

import System.Random

newtype Sample = Sample (Fixed E32 Int64)
  deriving ( Show
           , Read
           , Eq
           , Ord
           , Bounded
           , Enum
           , Num
           , Fractional
           , Real
           , RealFrac )

instance Floating Sample where
  pi    = fromRational 3.141592653589793
  
  exp   = undefined
  log   = undefined

  -- Stolen from
  -- https://github.com/kennyalive/fast-sine-cosine/blob/master/src/main.cpp

  sin w = y'
    where
      x  = ((w+pi) `fmod` (2*pi)) - pi
      b  = 4/pi
      c  = -4/(pi*pi)
      p  = 0.225
      y  = b * x + c * x * abs x
      y' = p * (y * abs y - y) + y
  
  cos x = sin $ pi/2 - x
  
  asin  = undefined
  acos  = undefined
  atan  = undefined
  sinh  = undefined
  cosh  = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined

instance Random Sample where
  random g = (r, g')
    where
      r = fromIntegral i / m
      (i, g') = randomR (0::Int64, floor m) g
      m = maxBound
      
  randomR (lo,hi) g = (r', g')
    where
      r' = lo + r * (hi-lo)
      (r, g') = random g

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

fmod :: RealFrac a => a -> a -> a
fmod a b = a - n*b
  where n = fromIntegral $ floor $ a / b

