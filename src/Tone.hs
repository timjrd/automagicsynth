module Tone where

import Control.Monad.Random

import Util
import Noise

data Patch = Patch Int [(Rational,Rational)] [(Rational,Rational)]
data Tone  = Tone  Int [(Sample,Sample)]

minHz  = 20
maxHz  = 20000
fromHz = minHz

somePatch = iterate nextPatch basePatch !! 32
someTone  = patch somePatch

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
      _ -> error "tone: downsampling only"

patch :: Patch -> Tone
patch (Patch n ls rs) = Tone n
  $ zip (patch1 n ls) (patch1 n rs)

patch1 :: Int -> [(Rational,Rational)] -> [Sample]
patch1 n xs = normalize
  $ take (samples (fromIntegral n / fromHz) - 1)
  $ sample $ \t -> foldr (f t) 0 xs
  where
    f t (hz,amp) s = s + fromRational amp *
      sin (2*pi * t * fromHz * fromRational hz)    

normalize :: [Sample] -> [Sample]
normalize xs = map f xs
  where
    (x1,y1) = (minimum xs, -1)
    (x2,y2) = (maximum xs,  1)
    
    d = y1 - k * x1
    k = (y2 - y1) / (x2 - x1)
    
    f x = d + k * x

basePatch = Patch 2 xs xs
  where
    n   = 32
    xs  = map f [0..n-1]
    f i = (hz,amp)
      where
        hz  = fromIntegral $ (i `div` 2) + 1
        amp = 1 / (k*20+1)
        k   = fromIntegral i / fromIntegral (n-1)

nextPatch (Patch n ls rs) = Patch n (nextPatch1 0 ls) (nextPatch1 1 rs)

nextPatch1 :: Int -> [(Rational,Rational)] -> [(Rational,Rational)]
nextPatch1 c = zipWith f [0::Int ..]
  where
    f i (hz,amp) = ( max 1 $ hz  + sHz + dHz
                   , max 0 $ amp + amp * sAmp )
      where
        h1 = c <#> i <#> hz <#> amp
        h2 = hash h1
        h3 = hash h2
        
        r1 = noise h1 :: Rational
        r2 = noise h2 :: Rational
        r3 = noise h3 :: Rational
        
        pmHz  = 0.004
        pmAmp = 0.04
        
        sHz  = (r1 * 2 - 1) * pmHz
        sAmp = (r2 * 2 - 1) * pmAmp
        
        dHz  = if r3 < 0.1 then -1
          else if r3 < 0.2 then  1
                           else  0
        
