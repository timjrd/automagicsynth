module Tone where

import Control.Monad.Random

import Util

data Patch = Patch Int [(Rational,Rational)]
data Tone  = Tone  Int [(Sample,Sample)]

minHz  = 20
maxHz  = 20000
fromHz = minHz

someTone = let n = 1 in Tone n
  $ map dup
  $ take (samples (fromIntegral n / fromHz) - 1)
  $ sample
  $ \t -> sin (2*pi*fromHz*t)

tone :: Tone -> Sample -> [(Sample,Sample)]
tone (Tone n xs) hz = cycle $ f 0 1 xs
  where
    toHz        = max minHz $ min maxHz hz
    fromSamples = samples (fromIntegral n / fromHz) - 1
    toSamples   = samples (fromIntegral n / toHz  ) - 1
    j i         = floor $ fromIntegral i *
      (fromIntegral toSamples / fromIntegral fromSamples)
    
    f _ _        []  = []
    f _ m (x0   :[]) = [x0 / fromIntegral m]
    f i m (x0:x1:xs) = case j (i+1) - j i of
      0 -> f (i+1) (m+1) (x0+x1:xs)
      1 -> x0 / fromIntegral m :  f (i+1) 1 (x1:xs)
      _ -> [] -- error: downsampling only

patch :: Patch -> Tone
patch (Patch n xs) = Tone n $ normalize ys
  where
    ys = map dup
      $ take (samples (fromIntegral n / fromHz) - 1)
      $ sample
      $ \t -> foldr (f t) 0 xs
    
    f t (k,amp) s = s + fromRational amp *
      sin (2*pi * t * fromHz * fromRational k)

normalize :: [(Sample,Sample)] -> [(Sample,Sample)]
normalize xs = map f xs
  where
    minL = minimum $ map fst xs
    minR = minimum $ map snd xs
    maxL = maximum $ map fst xs
    maxR = maximum $ map snd xs
    
    (x1,y1) = ((minL,minR), (-1,-1))
    (x2,y2) = ((maxL,maxR), (1,1))
    
    d = y1 - k * x1
    k = (y2 - y1) / (x2 - x1)
    
    f x = d + k * x

-- data extracted from the C++ program attached to this file:
-- https://en.wikipedia.org/wiki/File:Additive_synthesis_bell.ogg
bell = Patch 1
  [ (01.0000, 00.1970),
    (01.6766, 00.0176),
    (02.1741, 00.0750),
    (02.2537, 00.0225),
    (02.3830, 00.0385),
    (02.4925, 00.0265),
    (03.3731, 00.1201),
    (04.4029, 00.1043),
    (05.4378, 00.0805),
    (06.1393, 00.0429),
    (02.7363, 00.0431),
    (06.9701, 00.0429),
    (08.4079, 00.0429),
    (08.8557, 00.0429),
    (09.9601, 00.0429),
    (12.1840, 00.0429),
    (13.1890, 00.0426),
    (14.3731, 00.0241),
    (14.9353, 00.0239),
    (16.1691, 00.0237),
    (18.4079, 00.0237) ]
