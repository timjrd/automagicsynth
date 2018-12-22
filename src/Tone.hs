module Tone where

import Control.Monad.Random hiding (fromList)

import Shared
import Noise
import Envelope
import Wavetable
import Play

import Graphics.Gnuplot.Simple

-- debug :: IO ()
-- debug = do
--   let hz = 100
--   tone <- randomTone
--   plotList []
--     $ map (toRational . fst)
--     -- $ period
--     $ sampleR 0 (2/hz)
--     $ synth someTone hz

fromLength = 512 :: Int -- 2^9
-- fromHz = sampleRate / fromIntegral fromLength

someTone = fromList 1 [samplePeriod f]
  where f x = dup $ sin $ 2*pi * x

someNote = Note
  { time      = 0
  , duration  = 1
  , envelope' = someEnvelope
  , tone      = someTone
  , velocity  = const 1
  , pitch     = const 440 }

randomSnare :: MonadRandom m => m Note
randomSnare = do
  dt        <- getRandomR (0.3, 0.6)
  fromPitch <- getRandomR (100, 300)
  toPitch   <- getRandomR (fromPitch, fromPitch*1.5)
  toNoise   <- getRandomR (0.4, 0.9)
  base      <- randomTone
  let tone' =  drum dt fromPitch toPitch 0.1 toNoise base
  
  attack <- getRandomR (0.001, 0.005)
  decay  <- getRandomR (dt*0.6, dt*0.9)
  return $ Note { time      = 0
                , duration  = dt
                , envelope' = Envelope attack decay 0 0
                , tone      = tone'
                , velocity  = const 1
                , pitch     = const 1 }

randomKick :: MonadRandom m => m Note
randomKick = do
  dt        <- getRandomR (0.4, 0.8)
  fromPitch <- getRandomR (77, 88)
  toPitch   <- getRandomR (fromPitch/2, fromPitch/4)
  base      <- randomTone
  let tone' =  drum dt fromPitch toPitch 0 0 base
  
  attack <- getRandomR (0.001, 0.005)
  decay  <- getRandomR (dt*0.6, dt*0.9)
  return $ Note { time      = 0
                , duration  = dt
                , envelope' = Envelope attack decay 0 0
                , tone      = tone'
                , velocity  = const 1
                , pitch     = const 1 }

drum :: Number -> Number -> Number -> Number -> Number -> Wavetable -> Wavetable
drum dt fromPitch toPitch fromNoise toNoise tone =
  fromList dt [f tone n]
  where
    n = samples dt
    f _ 0 = []
    f x i = v : f y (i-1)
      where
        t = fromIntegral (n-i) / sampleRate
        k = fromIntegral (n-i) / fromIntegral (n-1)

        pitch     = (1-k) * fromPitch + k * toPitch
        noiseRate = dup $ (1-k) * fromNoise + k * toNoise
        
        (y,u) = synth x pitch t
        v     = (1-noiseRate) * u + noiseRate * noise u

randomTone :: MonadRandom m => m Wavetable
randomTone = do
  n  <- getRandomR (1,8)
  dt <- getRandomR (1 / fromIntegral n, fromIntegral n)
  ps <- replicateM n randomPeriod
  return $ fromList dt ps

randomPeriod :: MonadRandom m => m [(Sample,Sample)]
randomPeriod = do
  ls <- addSynth <$> randomHarmonics
  rs <- addSynth <$> randomHarmonics
  return $ zip ls rs

randomHarmonics :: MonadRandom m => m [(Int,Number,Number)]
randomHarmonics = do
  n    <- getRandomR (0,15)
  hzs  <- take n <$> getRandomRs (1,10)
  pss  <- take n <$> getRandomRs (0,pi)
  amps <- take n <$> getRandomRs (0.1, 0.9)
  return $ zip3 (1:hzs) (0:pss) (1:amps)

addSynth :: [(Int,Number,Number)] -> [Sample]
addSynth xs = normalize
  $ samplePeriod
  $ \x -> foldr (f x) 0 xs
  where
    f x (hz,ps,amp) s = s + amp *
      sin (ps + 2*pi * x * fromIntegral hz)

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

samplePeriod :: (Number -> b) -> [b]
samplePeriod f = map (f . (/fromIntegral fromLength) . fromIntegral)
                 [0..fromLength - 1]
