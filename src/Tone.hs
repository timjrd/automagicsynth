module Tone where

import Control.Monad.Random hiding (fromList)

import Shared
import Wavetable

fromLength = 512 -- 2^9
fromHz     = 1 / (samplePeriod * fromIntegral fromLength)

someTone = fromList 1 [take fromLength $ sample f]
  where f t = dup $ sin $ 2*pi * fromHz * t

normalize :: [Sample] -> [Sample]
normalize xs
  | x1 == x2  = map (const $ max (-1) $ min 1 x1) xs
  | otherwise = map f xs
  where
    (x1,y1) = (minimum xs, -1)
    (x2,y2) = (maximum xs,  1)

    d = y1 - k * x1
    k = (y2 - y1) / (x2 - x1)
    
    f x = d + k * x

    
