module Tone where

import Control.Monad.Random

import Noise
import Util

data Tone = Tone ()

patch = Tone ()

tone :: Tone -> Sample -> [(Sample,Sample)]
tone patch hz = dup <$> f 0 (-1)
  where f i x = 0.7 * x : f (i+1) (walk patch hz i x)

walk :: Tone -> Sample -> Int -> Sample -> Sample
walk patch hz i x = x + d * r * s
  where
    ra = abs $ noise1 x
    rs = noise1 ra
    r  = da * ra + (1-da) * rs
    s  = if i `mod` (n*2) < n then 1 else (-1)
    n  = samples (1/hz) `div` 2
    da = 0.8
    d  = 2 / (da * 0.5) / fromIntegral n
