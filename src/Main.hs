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

-- run time  : 1m35,313s
-- track time: 3m16s
-- speedup   : 2x

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

  -- let kick  = evalRand randomKick  $ mkStdGen 47 -- 47
  --     snare = evalRand randomSnare $ mkStdGen 55 -- 55

  kick  <- randomKick
  snare <- randomSnare
    
  let amps = concat
        [ replicate 2 [0.4 , 0   , 0   , 0   ]
        , replicate 3 [0.4 , 0.3 , 0   , 0   ]
        , replicate 4 [0.4 , 0.3 , 0.2 , 0   ] ]
        ++ repeat     [0.4 , 0.3 , 0.2 , 0.07]
  
  return $ concat $ zipWith4 (f kick snare) [0..] melodies tones amps
  
  where
    hz  = 440
    vol = 0.5
    dt  = 0.2
    n   = 32
    
    f :: Note -> Note -> Int -> [[Rational]] -> [Wavetable] -> [Number] -> [Note]
    f kick snare i melody' tone' amp = sort $ concat $ kicks : snares : withAmp
      where
        t0 = fromIntegral i * dt * n
        
        base = someNote { time = t0
                        , duration = dt
                        , envelope' = Envelope 0.01 0.02 0.5 0.2 }
        
        pitched = zipWith (map . (*)) [hz/4, hz/2, hz/2, hz] melody'
        
        voices = map (seqNotes . joinPitches base) pitched
        
        fused = zipWith (&) voices
          [id, id, id, fuse]
          where
            fuse = (map g) . fuseNotes 0.95
            g x = x { envelope' = Envelope 0.01 1 1 0.01 }
        
        withTone = zipWith (map . g) tone' fused
          where g w x = x { tone = w }
          
        withAmp = zipWith (map . g) amp withTone
          where g v x = x { velocity = const $ v * vol }

        kicks1 = rep 4 0     kick  { velocity = const 0.3 }
        kicks2 = rep 4 0.125 kick  { velocity = const 0.3 }
        kicks  = kicks1 ++ kicks2
        snares = rep 4 0.5 snare { velocity = const 0.2 }

        rep m i0 x = map g [0..m-1]
          where g i = x { time = t0 + dt*n/m * (i0 + i) }
