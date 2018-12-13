module Tone where

import Control.Monad.Random

import Noise
import Util

data Patch = Patch Int [Rational]
data Tone  = Tone  Int [(Sample,Sample)]

fromHz = 10

someTone = let n = 1 in Tone n
  $ map dup
  $ take (samples (fromIntegral n / fromHz) + 1)
  $ sample
  $ \t -> sin (2*pi*fromHz*t)

tone :: Tone -> Sample -> [(Sample,Sample)]
tone (Tone n xs) hz = cycle $ f 0 1 xs
  where
    fromSamples = samples (fromIntegral n / fromHz) + 1
    toSamples   = samples (fromIntegral n / hz    ) + 1
    j i         = floor $ fromIntegral i *
      (fromIntegral toSamples / fromIntegral fromSamples)
    
    f _ _        []  = []
    f _ m (x0   :[]) = [x0 / fromIntegral m]
    f i m (x0:x1:xs) = case j (i+1) - j i of
      0 -> f (i+1) (m+1) (x0+x1:xs)
      1 -> x0 / fromIntegral m :  f (i+1) 1 (x1:xs)
      d -> interpolate d x0 x1 ++ f (i+1) 1 (x1:xs)

interpolate :: Int -> (Sample,Sample) -> (Sample,Sample) -> [(Sample,Sample)]
interpolate n from to = map f [0..n-1]
  where
    f i = (1-k) * from + k * to
      where
        k = fromIntegral i / fromIntegral n
