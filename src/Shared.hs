module Shared
  ( module Shared
  , module Pair
  , module Fixed ) where

import Pair
import Fixed

type Number = Fixed
type Sample = Fixed

sampleRate   = 44100 :: Number
samplePeriod = 1 / sampleRate :: Number

samples :: Integral b => Number -> b
samples t = floor $ t * samplePeriod

sampleI :: Int -> (Number -> b) -> [b]
sampleI i f = sampleF f [i..]

sampleR :: Number -> Number -> (Number -> b) -> [b]
sampleR t0 tn f = sampleF f [samples t0 .. samples tn - 1]

sample :: (Number -> b) -> [b]
sample f = sampleF f [0..]

sampleF :: (Number -> b) -> [Int] -> [b]
sampleF f = map (f . (samplePeriod *) . fromIntegral)

ramp :: (Fractional a, Ord a) => a -> a -> a -> a -> a -> a
ramp fromX toX fromY toY t = max (min fromY toY) $ min (max fromY toY)
                             $ fromY + ((t-fromX) / (toX-fromX)) * (toY-fromY)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = x : splitEvery n xs'
  where (x,xs') = splitAt n xs
