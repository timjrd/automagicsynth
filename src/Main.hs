module Main where

import Data.Function ((&))
import Control.Arrow ((>>>))
import Control.Monad
import Data.List

import Control.Monad.Random
import System.Random.Shuffle

import Util

import Composition
import Tone
import Script
import PCM

import Graphics.Gnuplot.Simple

main :: IO ()
main = evalRandIO track >>= putPCM

track :: MonadInterleave m => m [(Sample,Sample)]
track = do
  let hz = 440
  
  cc <- interleave
    $ fmap concat
    $ sequence
    $ repeat
    $ fmap (replicate 4)
    $ melody

  tones <- interleave
    $ fmap (concatMap (replicate 3))  
    $ fmap (splitEvery 4)
    $ sequence
    $ repeat
    $ interleave randomTone

  let amps = concat
        [ replicate 2 [0.4 , 0   , 0   , 0   ]
        , replicate 3 [0.4 , 0.3 , 0   , 0   ]
        , replicate 4 [0.4 , 0.3 , 0.2 , 0   ] ]
        ++ repeat     [0.4 , 0.3 , 0.2 , 0.07]

  return
    $ take (samples 120)
    $ zipWith (*) (sample $ dup . ramp 0   0.5 0 1)
    $ zipWith (*) (sample $ dup . ramp 115 120 1 0)
    -- $ compress 0.8 4
    $ vol 0.6
    -- $ lowpass (fromRational hz * 8)
    $ synthScript
    $ line
    $ zipWith3
    ( \notes amp tone ->
        together
        $ zipWith3 (flip mergeNotes 0.11) tone amp
        $ zipWith (map . (*)) [hz/4, hz/2, hz/2, hz] notes )
    cc amps tones

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = x : splitEvery n xs'
  where (x,xs') = splitAt n xs
