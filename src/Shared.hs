module Shared
  ( module Shared
  , module Pair
  , module Fixed ) where

import Pair
import Fixed

type Number = Fixed
type Sample = Fixed

sampleRate   = 44100         :: Int
samplePeriod = fromSamples 1 :: Number

toSamples :: Number -> Int
toSamples t = (toIntBits t * sampleRate) `right` halfBits

fromSamples :: Int -> Number
fromSamples t = fromIntBits $ int + frac
  where
    seconds       = t `div` sampleRate
    int           = seconds `left` halfBits

    second1       = t `mod` sampleRate
    intSecond1    = second1    `left` halfBits
    intSampleRate = sampleRate `left` halfBits
    frac          = (intSecond1 `left` halfBits) `div` intSampleRate

sampleI :: Int -> (Number -> b) -> [b]
sampleI i f = sampleF f [i..]

sampleR :: Number -> Number -> (Number -> b) -> [b]
sampleR t0 tn f = sampleF f [toSamples t0 .. toSamples tn - 1]

sample :: (Number -> b) -> [b]
sample f = sampleF f [0..]

sampleF :: (Number -> b) -> [Int] -> [b]
sampleF f = map $ f . fromSamples

ramp :: (Fractional a, Ord a) => a -> a -> a -> a -> a -> a
ramp fromX toX fromY toY t = max (min fromY toY) $ min (max fromY toY)
                             $ fromY + ((t-fromX) / (toX-fromX)) * (toY-fromY)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = x : splitEvery n xs'
  where (x,xs') = splitAt n xs

