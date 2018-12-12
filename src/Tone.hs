module Tone (tone, someTone) where

import Control.Monad.Random

import Data.List

import Util
import Interpolable

data ToneR = ToneR
  { modFactor :: (Int,Int)
  , modShift  :: Sample
  , modRate   :: Sample
  , sumRate   :: Sample
  , depth     :: Int }

data ToneD = ToneD
  { modShiftD  :: Sample
  , modRateD   :: Sample
  , sumWeightD :: Sample }

data Tone a = Sin a a (Tone a)
            | Sum a a (Tone a) (Tone a)
            | Empty

instance Functor Tone where
  fmap f (Sin a b c  ) = Sin (f a) (f b) (fmap f c)
  fmap f (Sum a b c d) = Sum (f a) (f b) (fmap f c) (fmap f d)
  fmap _ Empty = Empty

(<#>) :: Tone (a -> b) -> Tone a -> Tone b
(Sin a1 b1 c1   ) <#> (Sin a2 b2 c2   ) = Sin (a1 a2) (b1 b2) (c1 <#> c2)
(Sum a1 b1 c1 d1) <#> (Sum a2 b2 c2 d2) = Sum (a1 a2) (b1 b2) (c1 <#> c2) (d1 <#> d2)
_ <#> _ = Empty

randomTone :: MonadRandom m => ToneR -> m (Tone Sample)
randomTone tr = randomTone' True 0
  where
    randomTone' d0 d
      | d == depth tr = randomSin d0 d
      | d >  depth tr = return Empty
      | otherwise = do
          r <- getRandom
          if r < sumRate tr
            then randomSum d0 d
            else randomSin d0 d
    
    randomSum d0 d = do
      w1 <- getRandom
      w2 <- getRandom
      f1 <- randomTone' d0 (d+1)
      f2 <- randomTone' d0 (d+1)
      return $ Sum w1 w2 f1 f2

    randomSin d0 d = do
      mf <- randomFactor d0
      mr <- getRandomR (-modRate tr, modRate tr)
      f1 <- randomTone' False (d+1)
      return $ Sin mf mr f1

    randomFactor True  = return 1
    randomFactor False = do
      mf <- getRandomR $ modFactor tr
      ms <- getRandomR (-modShift tr, modShift tr)
      return $ fromIntegral mf + ms

randomToneD :: MonadRandom m => ToneD -> (Tone Sample) -> m (Tone Sample)
randomToneD _  Empty = return Empty
randomToneD td (Sin mf mr f1) = do
  df  <- getRandomR (-modShiftD td, modShiftD td)
  dr  <- getRandomR (-modRateD  td, modRateD  td)
  f1' <- randomToneD td f1
  return $ Sin (mf+df) (mr+dr) f1'

randomToneD td (Sum w1 w2 f1 f2) = do
  d1  <- getRandomR (-sumWeightD td, sumWeightD td)
  d2  <- getRandomR (-sumWeightD td, sumWeightD td)
  f1' <- randomToneD td f1
  f2' <- randomToneD td f2
  return $ Sum (w1+d1) (w2+d2) f1' f2'

tone :: Tone (Sample -> Sample) -> (Sample -> Sample) -> [Sample]
tone Empty _ = repeat 0
tone (Sin mf mr f1   ) hz = fm (\t -> hz t * mf t) mr (tone f1 hz)
tone (Sum w1 w2 f1 f2) hz = add w1 w2 (tone f1 hz) (tone f2 hz)

add :: (Sample -> Sample) -> (Sample -> Sample) -> [Sample] -> [Sample] -> [Sample]
add wf1 wf2 xs1 xs2 = zipWith4 f (sample wf1) (sample wf2) xs1 xs2
  where f w1 w2 x1 x2 = (x1*w1 + x2*w2) / (w1+w2)

fm :: (Sample -> Sample) -> (Sample -> Sample) -> [Sample] -> [Sample]
fm hz mr = f 0 0
  where
    f _ _    []  = []
    f i a (x:xs) = sin a : f (i+1) a' xs
      where
        t  = i * fromRational frame
        a' = a + 2*pi * fi * fromRational frame
        fi = hz t + hz t * mr t * x

pan :: Sample -> (Sample,Sample) -> (Sample,Sample)
pan p (l,r) = ( l * (1-rp) + r * lp
              , r * (1-lp) + l * rp )
  where
    lp = abs (min p 0)
    rp = max p 0

someTone :: Tone (Sample -> Sample)--, Tone (Sample -> Sample))
someTone = flip evalRand (mkStdGen 105) $ do
  toneL <- randomTone $ ToneR
    { modFactor = (1,5)
    , modShift  = 0.1
    , modRate   = 0.8
    , sumRate   = 0.5
    , depth     = 5 }
    
  -- toneR <- flip randomToneD toneL $ ToneD
  --   { modShiftD  = 0.05
  --   , modRateD   = 0.05
  --   , sumWeightD = 0.05 }

  -- return ( fmap const toneL
  --        , fmap const toneR )

  return $ fmap const toneL
  
