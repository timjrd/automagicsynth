module Main where

import Data.Function ((&))

import Control.Monad
import Data.List

import Control.Monad.Random

import Util
import Composition
import Tone
import Envelope
import Wavetable
import Play
import Render
import Rhythm

-- run time  : 1m35,313s
-- track time: 3m16s
-- speedup   : 2x

-- main = evalRandIO beats >>= putRaw 4096 play . initNotes
main = evalRandIO track >>= putRaw 4096 play . initNotes
-- main = putRaw 4096 (purePlayer $ \t -> dup $ sin $ 2*pi * 440 * t) ()

beats :: MonadInterleave m => m [Note]
beats = do
  kick  <- mkDrum <$> randomKick
  snare <- mkDrum <$> randomSnare
  kr    <- rhythm 7 2
  sr    <- rhythm 5 2
  
  let kn = length kr
      sn = length sr

  return $ cycle' $ sort $
    concatMap (beat kn kick) (zip [1..] kr)
    ++ concatMap (beat sn snare) (zip [1..] sr)
  
  where
    dt = 6
    
    beat _ _  (_,False) = []
    beat n xs (i,True)  = map f xs
      where
        f x = x { time = dt * fromIntegral i / fromIntegral n
                , velocity = (*0.5) . velocity x }

    cycle' = concatMap f . zip [0..] . repeat
      where
        f (i,xs) = map g xs
          where
            g x = x { time = time x + dt * fromIntegral i }
  
track :: MonadInterleave m => m [Note]
track = do  
  melodies' <- melodies
  
  tones <- randomTone 400 33
    & fmap mkTone
    & repeat
    & sequence
    & fmap (chunksOf 5)
    & fmap (concatMap (replicate 3))
    & interleave
  
  kicks <- randomKick
    & fmap mkDrum
    & fmap (replicate 3)
    & repeat
    & sequence
    & fmap concat
    & interleave
  
  snares <- randomSnare
    & fmap mkDrum
    & fmap (replicate 4)
    & repeat
    & sequence
    & fmap concat
    & interleave

  attacks <- concatMap (replicate 2)
    <$> chunksOf 5
    <$> getRandomRs (0.001, 0.03)
    
  let amps = concat
        [ replicate 2  [0   , 0   , 0   , 0   , 0.2]
        , replicate 4  [0.35, 0   , 0   , 0   , 0.2]
        , replicate 8  [0.35, 0.3 , 0   , 0   , 0.2]
        , replicate 16 [0.35, 0.3 , 0.25, 0   , 0.2]
        , replicate 32 [0.35, 0.3 , 0.25, 0.15, 0.2]
        , replicate 4  [0.35, 0.3 , 0.25, 0.15, 0  ]
        , replicate 2  [0.35, 0.3 , 0.25, 0   , 0  ]
        , replicate 1  [0.35, 0.3 , 0   , 0   , 0  ]
        , replicate 2  [0.35, 0   , 0   , 0   , 0  ] ]
  
  let track' = concat $ zipWith7 f [0..] melodies' tones amps kicks snares attacks
  
  return $ track' ++ [someNote {time = 60*60, velocity = const 0}]
  
  where
    hz  = 440
    vol = 0.9
    dt  = 0.2
    n   = 16
    
    f i melody' tone' amp kick snare attacks = sort $ concat $ withAmp -- : kicks : snares
      where
        t0 = fromIntegral i * dt * n
        
        base = someNote { time         = t0
                        , duration     = dt
                        , noteEnvelope = Envelope 0.01 0.05 0.5 0.55 }
        
        pitched = zipWith (map . (*)) [hz/4, hz/2, hz/2, hz, hz] melody'
        
        voices = map (seqNotes . joinPitches base) pitched

        fused = zipWith (&) voices
          [id, id, id, fuse, id]
          where
            fuse = (map g) . fuseNotes 0.65
            g x = x { noteEnvelope = Envelope 0.01 1 1 0.01 }
        
        withAttack = zipWith (map . f) attacks fused
          where
            f a x = x { noteEnvelope = (noteEnvelope x) { attack = a } }
        
        withTone = zipWith (map . g) tone' withAttack
          where g w x = x { tone = w }
          
        withAmp = zipWith (map . g) amp withTone
          where g v x = x { velocity = const $ v * vol }

        kicks1 = rep 4 0    0.3 kick
        kicks2 = rep 4 0.25 0.3 kick
        kicks  = kicks1 ++ kicks2
        snares = rep 4 0.5 0.2 snare

        rep m i0 v xs = concatMap (flip map xs . g) [0..m-1]
          where g i x = x { time = t0 + dt*n/m * (i0 + i)
                          , velocity = (v*) . velocity x }
