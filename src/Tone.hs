module Tone where

import Control.Monad.Random hiding (fromList)

import Data.List

import Shared
import Noise
import Envelope
import Wavetable
import Play

import Graphics.Gnuplot.Simple

data Tone = Tone Number [[ ((Number,Number), (Number,Number)) ]]
data Drum = Drum { fromPitch  :: Number
                 , toPitch    :: Number
                 , fromNoise  :: Number
                 , toNoise    :: Number
                 , drumAttack :: Number
                 , drumDecay  :: Number
                 , drumTone   :: Tone }

debug = do
  r <- randomTone
  plotList []
    $ take (samples $ 2/hz)
    $ map (toRational . fst)
    $ flip toList' (const hz)
    $ mkTone r
  where hz = 100

fromLength = 512 :: Int -- 2^9
fromHz = sampleRate / fromIntegral fromLength

someTone = fromList 1 [samplePeriod f]
  where f x = dup $ sin $ 2*pi * x

noiseTone = fromList 1 [samplePeriod noise]

someNote = Note
  { time         = 0
  , duration     = 1
  , noteEnvelope = someEnvelope
  , tone         = someTone
  , velocity     = const 1
  , pitch        = const 440 }

randomKick :: MonadRandom m => m Drum
randomKick = do
  fromPitch'  <- getRandomR (80,90)
  toPitch'    <- getRandomR (fromPitch'/1.5, fromPitch'/2)
  drumAttack' <- getRandomR (0.001, 0.01)
  drumDecay'  <- getRandomR (0.5, 3)
  drumTone'   <- randomTone
  return Drum { fromPitch  = fromPitch'
              , toPitch    = toPitch'
              , fromNoise  = 0
              , toNoise    = 0
              , drumAttack = drumAttack'
              , drumDecay  = drumDecay'
              , drumTone   = drumTone' }

randomSnare :: MonadRandom m => m Drum
randomSnare = do
  fromPitch'  <- getRandomR (100,300)
  toPitch'    <- getRandomR (fromPitch'/1.1, fromPitch'/1.4)
  fromNoise'  <- getRandomR (0.01, 0.2)
  toNoise'    <- getRandomR (0.3, 1)
  drumAttack' <- getRandomR (0.001, 0.01)
  drumDecay'  <- getRandomR (0.5, 3)
  drumTone'   <- randomTone
  return Drum { fromPitch  = fromPitch'
              , toPitch    = toPitch'
              , fromNoise  = fromNoise'
              , toNoise    = toNoise'
              , drumAttack = drumAttack'
              , drumDecay  = drumDecay'
              , drumTone   = drumTone' }

mkDrum :: Drum -> [Note]
mkDrum x = [toneNote, noiseNote]
  where
    dt  = drumAttack x + drumDecay x
    env = Envelope (drumAttack x) (drumDecay x) 0 0
    toneNote = Note
      { time         = 0
      , duration     = dt
      , noteEnvelope = env
      , tone         = mkTone (drumTone x)
      , velocity     = ramp 0 dt (1-fromNoise x) (1-toNoise x)
      , pitch        = ramp 0 dt (fromPitch x) (toPitch x) }
    noiseNote = Note
      { time         = 0
      , duration     = dt
      , noteEnvelope = env
      , tone         = noiseTone
      , velocity     = ramp 0 dt (fromNoise x) (toNoise x)
      , pitch        = const 440 }
        
randomTone :: MonadRandom m => m Tone
randomTone = do
  m    <- getRandomR mRange
  dt   <- getRandomR ( fromIntegral m * fst dtRange
                     , fromIntegral m * snd dtRange )
  subs <- replicateM m randomSubTone
  return $ Tone dt subs
  where
    mRange  = (2,8)
    dtRange = (0.3, 2)

randomSubTone :: MonadRandom m => m [ ((Number,Number), (Number,Number)) ]
randomSubTone = do
  n <- getRandomR nRange
  replicateM n randomControl
  where
    nRange = (1,3)

randomControl :: MonadRandom m => m ((Number,Number), (Number,Number))
randomControl = do
  l3 <- getRandomR (-1,1)
  l1 <- getRandomR (l3 - roughness, l3 + roughness)

  r3 <- getRandomR (-1,1)
  r1 <- getRandomR (r3 - roughness, r3 + roughness)
  
  return ((l1,l3), (r1,r3))
  where
    roughness = 0.1

mkTone :: Tone -> Wavetable
mkTone (Tone dt ys) = fromList dt
  $ map (uncurry zip . both bezierSynth . unzip) ys
  
-- See:
-- https://math.stackexchange.com/a/2571749
-- http://www.algosome.com/articles/continuous-bezier-curve-line.html

bezierSine = [(1,0), (0,1)]

bezierSynth :: [(Number,Number)] -> [Sample]
bezierSynth ys = normalize
  $ samplePeriod
  $ reduce
  $ polybezier n
  $ cycle ys
  where
    n = length ys
    d = 1 / fromIntegral n

    err = error "bezierSynth: empty list"
    polybezier _      []  = err
    polybezier _ (  _:[]) = err
    polybezier _ (_:_:[]) = err

    polybezier 0 _ = []
    polybezier i ( (_,y0) : a@(y1,y3) : b@(b1,_) : ys ) =
      bezier y0 y1 y2 y3 . f : polybezier (i-1) (a:b:ys)
      where
        y2  = 2 * y3 - b1
        f x = max 0 $ min 1 $
          (x - (fromIntegral (n-i) * d)) / d
    
    reduce fs x = foldl' (\s f -> s + f x) 0 fs
        
bezier y0 y1 y2 y3 x =
  pow3 (1-x) * y0
  + 3 * x * pow2 (1 - x) * y1
  + 3 * pow2 x * (1 - x) * y2
  + pow3 x * y3
  where
    pow2 x = x*x
    pow3 x = x*x*x

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
