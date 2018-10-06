module Main where

import Data.Function ((&))
import Control.Arrow ((>>>))
import Control.Monad
import Data.List

import Control.Monad.Random
import System.Random.Shuffle

import Util

import Composition
import Noise
import Script
import PCM

import Graphics.Gnuplot.Simple

main :: IO ()
main = evalRandIO track >>= putPCM

track :: MonadInterleave m => m [(Sample,Sample)]
track = do
  let hz = 440

  cc <- fmap concat
    $ sequence
    $ repeat
    $ fmap (replicate 3)
    $ melody

  return
    -- $ take (samples 120)
    -- $ zipWith (*) (sample $ dup . ramp 0   5   0 1)
    -- $ zipWith (*) (sample $ dup . ramp 115 120 1 0)
    $ compress 0.8 4
    $ vol 0.7
    $ lowpass (fromRational hz * 8)
    $ synthScript
    $ line
    $ map ( together
            . zipWith (mergeNotes 0.17) [0.4 , 0.3 , 0.2 , 0.1]
            . zipWith (map . (*))       [hz/4, hz/2, hz/2, hz ] )
    $ cc
