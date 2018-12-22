module Main where

import Data.Function ((&))

import Control.Monad
import Data.List

import Control.Monad.Random

import Shared

import Composition
import Tone
import Envelope
import Wavetable
import Play
import Render

main = evalRandIO track >>= putRaw 4096 play . initNotes

-- main = putRaw 4096 (purePlayer $ \t -> dup $ sin $ 2*pi * 440 * t) ()

track :: MonadInterleave m => m [Note]
track = do  
  melodies <- melody
    & fmap (replicate 4)
    & repeat
    & sequence
    & fmap concat
    & interleave
  
  tones <- randomTone
    & repeat
    & sequence
    & fmap (splitEvery 4)
    & fmap (concatMap (replicate 3))
    & interleave
  
  let amps = concat
        [ replicate 2 [0.4 , 0   , 0   , 0   ]
        , replicate 3 [0.4 , 0.3 , 0   , 0   ]
        , replicate 4 [0.4 , 0.3 , 0.2 , 0   ] ]
        ++ repeat     [0.4 , 0.3 , 0.2 , 0.08]
  
  return $ concat $ zipWith4 f [0..] melodies tones amps
  
  where
    hz  = 440
    vol = 0.5
    dt  = 0.15
    n   = 32
    
    f :: Int -> [[Rational]] -> [Wavetable] -> [Number] -> [Note]
    f i melody' tone' amp = sort $ concat withAmp
      where
        base = someNote { time = fromIntegral i * dt * n
                        , duration = dt
                        , envelope' = Envelope 0.01 0.02 0.5 0.3 }

        pitched = zipWith (map . (*)) [hz/4, hz/2, hz/2, hz] melody'

        voices = map (seqNotes . joinPitches base) pitched

        withTone = zipWith (map . g) tone' voices
          where g w x = x { tone = w }
          
        withAmp = zipWith (map . g) amp withTone
          where g v x = x { velocity = const $ v * vol }
