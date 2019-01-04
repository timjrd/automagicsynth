module Rhythm (rhythm) where

import Control.Monad.Random
import System.Random.Shuffle

import Util

divide :: MonadRandom m => Int -> Int -> m [Int]
divide dividend divisor =
  shuffleM remainder >>= return . zipWith (+) quotients
  where
    quotients = replicate divisor (dividend `div` divisor)
    remainder = replicate (dividend `mod` divisor) 1
      ++ replicate (divisor - (dividend `mod` divisor)) 0

rhythm :: MonadRandom m => Int -> Int -> m [Bool]
rhythm dividend divisor = fmap (map toEnum)
  $ divide dividend divisor >>= f
  where
    f xs | any (>1) xs = concatMapM g xs >>= f
         | otherwise   = return xs
    -- g 1  = return $ 1 : replicate (divisor-1) 0
    g n  = divide n divisor

