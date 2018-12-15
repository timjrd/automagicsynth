module Tone where

import Control.Monad.Random

import Data.Array

import Util
import Noise

-- Wavetable data type
data Tone = Tone Sample (Array Int (Array Int (Sample,Sample)))

fromLength = 512 -- 2^9
fromHz     = fromRational $ 1 / (frame * fromIntegral fromLength)

someTone = evalRand randomTone (mkStdGen 21)
-- good seeds: 21 33 35

sineTone = Tone 1 $ listArray (0,0)
  [ listArray (0,fromLength-1)
    $ sample
    $ \t -> dup $ sin $ 2*pi * fromHz * t ]

-- Wavetable synthesizer with linear interpolation
tone :: Tone -> Sample -> [(Sample,Sample)]
tone (Tone dt table) hz = sample f
  where
    n   = 1 + snd (bounds table)
    f t = c
      where
        i  = (t / dt) * fromIntegral n
        j  = (t * hz) * fromIntegral fromLength

        di = dup $ i `fmod` 1
        dj = dup $ j `fmod` 1

        i0 = floor   i `mod` n
        i1 = ceiling i `mod` n
        
        j0 = floor   j `mod` fromLength 
        j1 = ceiling j `mod` fromLength

        a0 = table ! i0 ! j0
        a1 = table ! i0 ! j1

        b0 = table ! i1 ! j0
        b1 = table ! i1 ! j1

        a  = (1-dj) * a0 + dj * a1
        b  = (1-dj) * b0 + dj * b1

        c  = (1-di) * a + di * b
      
normalize :: [Sample] -> [Sample]
normalize xs = map f xs
  where
    (x1,y1) = (minimum xs, -1)
    (x2,y2) = (maximum xs,  1)
    
    d = y1 - k * x1
    k = (y2 - y1) / (x2 - x1)
    
    f x = d + k * x

noiseWave :: MonadRandom m => m [Sample]
noiseWave = take fromLength <$> getRandomRs (-1,1)

smoothWave :: Int -> [Sample] -> [Sample]
smoothWave passes xs = normalize
  $ take fromLength
  $ drop (fromLength*padding) padded'
  where
    padding = 8
    padded  = concat $ replicate (padding*2+1) xs
    padded' = foldr lowpass1 padded $ map fromIntegral hzs
    
    minHz = 20
    maxHz = 20000
    hzs   = map f [0 .. passes-1]
    f i   = minHz + (((maxHz - minHz) * i) `div` (passes-1))

randomTone :: MonadRandom m => m Tone
randomTone = do
  base    <- noiseWave
  noisesL <- replicateM n noiseWave
  noisesR <- replicateM n noiseWave
  passes  <- getRandomRs (20,180)
  dt      <- getRandomR (0.1 * fromIntegral n, 2 * fromIntegral n)
  
  let basesL = map (zipWith shiftDT base) noisesL
      basesR = zipWith (zipWith shiftLR) basesL noisesR
      smoothesL = zipWith ($) (map smoothWave passes) basesL
      smoothesR = zipWith ($) (map smoothWave passes) basesR
      smoothes = zipWith zip smoothesL smoothesR
      
  return $ wavetable dt smoothes
  
  where
    n = 4
    shiftDT = shift 0.9
    shiftLR = shift 0.9
    shift k x d = (x + k * d) / (1 + k)

wavetable :: Sample -> [[(Sample,Sample)]] -> Tone
wavetable dt table = Tone dt
  $ listArray (0, length table - 1)
  $ map (listArray (0, fromLength-1)) table
