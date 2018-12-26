module Tone where

import Control.Monad.Random hiding (fromList)

import Data.Function ((&))
import Data.List

import Shared
import Constraint
import Noise
import Envelope
import Wavetable
import Play

import Graphics.Gnuplot.Simple

data Tone = Tone Number [ ([Bool],[Bool]) ]
data Drum = Drum { fromPitch  :: Number
                 , toPitch    :: Number
                 , fromNoise  :: Number
                 , toNoise    :: Number
                 , drumAttack :: Number
                 , drumDecay  :: Number
                 , drumTone   :: Tone }

debug = do
  r <- evalRandIO randomTone
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

randomKick :: MonadInterleave m => m Drum
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

randomSnare :: MonadInterleave m => m Drum
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
        
randomTone :: MonadInterleave m => m Tone
randomTone = do
  m    <- getRandomR (2,8)
  dt   <- getRandomR ( fromIntegral m * 0.1
                     , fromIntegral m * 2 )
  subs <- replicateM m randomSubTone
  return $ Tone dt subs

randomSubTone :: MonadInterleave m => m ([Bool],[Bool])
randomSubTone = do
  n  <- getRandomR (1,32)
  let steps = 2*n
  ls <- randomWalk steps
  rs <- randomWalk steps
  return (ls,rs)

randomWalk :: MonadInterleave m => Int -> m [Bool]
randomWalk steps = head <$> solve steps 1 (walk steps)

walk :: Int -> Constraint (Int,Int) () Bool
walk steps = Constraint (steps, 0) (repeat id) (repeat f)
  where
    f (1,-1) _ = [((0,0), True )]
    f (1, 1) _ = [((0,0), False)]
    f (1, _) _ = []
    f (i, x) _
      | abs x > i = []
      | otherwise = [ ((i-1, x+1), True )
                    , ((i-1, x-1), False) ]

mkTone :: Tone -> Wavetable
mkTone (Tone dt ds) = fromList dt
  $ map (uncurry zip . both polysinPeriod) ds
  
polysinPeriod :: [Bool] -> [Sample]
polysinPeriod = normalize . samplePeriod . polysin

polysin :: [Bool] -> (Number -> Number)
polysin [] = error "polysin: empty list"
polysin ds = f points . ramp 0 1 0 mx
  where
    points = cycle ds
      & dropWhile (== head ds)
      & take (length ds)
      & toOrdinates
      & skipOrdinates
      & toPoints

    mx = fst $ last points
    
    f (a:b:s) = \x -> polysinSlice a b x + f (b:s) x
    f _ = const 0

polysinSlice :: (Number,Number) -> (Number,Number) -> (Number -> Number)
polysinSlice (x0,y0) (x1,y1) =
  ramp (-1) 1 (min y0 y1) (max y0 y1)
  . sin
  . ramp x0 x1 t0 t1
  where
    (t0,t1) = if y0 < y1
      then (-pi/2,  pi/2) -- up
      else ( pi/2, -pi/2) -- down

toPoints :: [Int] -> [(Number,Number)]
toPoints = ((0,0):) . f 0
  where
    f x0 (y0:y1:ys) = (x1, fromIntegral y1) : f x1 (y1:ys)
      where
        dy = fromIntegral $ abs $ y1 - y0
        k  = 2 * acos ((dy-1)/dy) / pi
        x1 = x0 + k*dy
    f _ _ = []

toOrdinates :: [Bool] -> [Int]
toOrdinates = (0:) . snd . mapAccumL f 0
  where f x True  = dup $ x + 1
        f x False = dup $ x - 1

skipOrdinates :: [Int] -> [Int]
skipOrdinates ( x0 : x1 : x2 : xs )
  | signum (x1-x0) == signum (x2-x1) = skipOrdinates (x0 : x2 : xs)
  | otherwise = x0 : skipOrdinates (x1 : x2 : xs)
skipOrdinates xs = xs
             
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
