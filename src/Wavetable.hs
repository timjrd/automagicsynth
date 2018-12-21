module Wavetable
  ( Wavetable
  , fromList
  , synth ) where

import Data.List

import qualified Data.Vector.Unboxed as U

import Shared

type Boxed   = Fixed
type Unboxed = Int32
box   = fromIntBits
unbox = toIntBits

data Wavetable = Wavetable Boxed Int Int (U.Vector Unboxed)

fromList :: Boxed -> [[(Boxed,Boxed)]] -> Wavetable
fromList dt xs = Wavetable dt m n ys
  where
    m  = length xs
    n  = length $ head xs
    ys = U.fromList
      $ concatMap (\(l,r) -> [unbox l, unbox r])
      $ concat
      $ transpose xs

(!) :: Wavetable -> (Int,Int) -> (Boxed,Boxed)
(!) (Wavetable _ m _ xs) (i,j) = (box l, box r)
  where
    l = U.unsafeIndex xs (k*2)
    r = U.unsafeIndex xs (k*2+1)
    k = j * m + i

synth :: Wavetable -> Boxed -> Int -> (Boxed,Boxed)
synth table@(Wavetable dt m n _) hz t = c
  where
    -- i  = fromIntegral (t `mod` toSamples dt    ) / dt * fromIntegral m
    -- j  = fromIntegral (t `mod` toSamples (1/hz)) * hz * fromIntegral n
    
    i  = fromSamples t / dt * fromIntegral m
    j  = fromSamples t * hz * fromIntegral n
    
    di = dup $ i - fromIntegral (floor i)
    dj = dup $ j - fromIntegral (floor j)
    
    i0 = floor   i `mod` m
    i1 = ceiling i `mod` m
    
    j0 = floor   j `mod` n
    j1 = ceiling j `mod` n
    
    a0 = table ! (i0,j0)
    a1 = table ! (i0,j1)
    
    b0 = table ! (i1,j0)
    b1 = table ! (i1,j1)
    
    a  = (1-dj) * a0 + dj * a1
    b  = (1-dj) * b0 + dj * b1
    
    c  = (1-di) * a + di * b

