module Util
  ( module Util
  , dup
  , both
  , fmod ) where

import Data.Ratio
import Data.Bits
import Data.List

import System.Random

import Debug.Trace

import Fixed
import Pair

-- Float  x1
-- Double x1.4
-- Fixed  x2.6

type Number = Fixed
type Sample = Fixed

sampleRate :: Number
sampleRate = 44100

samples :: Integral b => Number -> b
samples t = floor $ t * sampleRate

instance (Integral a, Random a) => Random (Ratio a) where
  random g = (fromIntegral n % fromIntegral d, g')
    where
      n = abs r0   :: Int
      d = maxBound :: Int
      (r0,g') = random g

  randomR (lo,hi) g = (lo + (hi-lo) * r, g')
    where (r,g') = random g
  
ramp :: (Fractional a, Ord a) => a -> a -> a -> a -> a -> a
ramp fromX toX fromY toY t = max (min fromY toY) $ min (max fromY toY)
                             $ fromY + ((t-fromX) / (toX-fromX)) * (toY-fromY)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromIntegral (floor x)

isPowerOfTwo :: (Ord a, Num a, Bits a) => a -> Bool
isPowerOfTwo x = x > 0 && (x .&. (x - 1)) == 0

isPowerOfTwo' :: RealFrac a => a -> Bool
isPowerOfTwo' x = isInt x && isPowerOfTwo (toInteger $ floor x)

debug msg x = trace ("[DEBUG] " ++ msg ++ ": " ++ show x) x

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where (a,b) = splitAt n xs

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = mapM f xs >>= return . concat

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy $ \x y -> f x == f y

minimumOn :: (Ord b, Foldable t) => (a -> b) -> t a -> a
minimumOn f = minimumBy $ \x y -> compare (f x) (f y)
