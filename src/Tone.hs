module Tone where

import Control.Monad.Random hiding (fromList)

import Shared
import Wavetable

import Graphics.Gnuplot.Simple

debug :: IO ()
debug = do
  let hz = 100
  tone <- randomTone
  plotList []
    $ map (toRational . fst)
    -- $ period
    $ sampleR 0 (2/hz)
    $ synth someTone hz

fromLength = 512 :: Int -- 2^9
-- fromHz = sampleRate / fromIntegral fromLength

someTone = fromList 1 [samplePeriod f]
  where f t = dup $ sin $ 2*pi * t

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

randomHarmonics :: MonadRandom m => m [(Int,Number)]
randomHarmonics = do
  n    <- getRandomR (0,8)
  hzs  <- take n <$> getRandomRs (1,8)
  amps <- take n <$> getRandomRs (0.1,0.7)
  return $ zip (1:hzs) (1:amps)

addSynth :: [(Int,Number)] -> [Sample]
addSynth xs = normalize
  $ samplePeriod
  $ \t -> foldr (f t) 0 xs
  where
    f t (hz,amp) s = s + amp *
      sin (2*pi * t * fromIntegral hz)

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
