module Util where

import Data.Ratio
import Data.Bits
import Data.List

import System.Random

import Debug.Trace

frameRate :: Rational
frameRate = 44100

frame :: Rational
frame = 1 / frameRate

samples :: (RealFrac a, Integral b) => a -> b
samples t = floor $ fromRational frameRate * t

-- epsilon :: Rational
-- epsilon = 0.001

type Sample = Float

data Split a = Fst a
             | Snd a

fromSplit :: Split a -> a
fromSplit (Fst x) = x
fromSplit (Snd x) = x

fstLength :: [Split a] -> Int
fstLength (Fst x:xs) = 1 + fstLength xs
fstLength _          = 0

-- instance Integral a => Sampleing (Ratio a) where
--   pi    = fromRational $ F.pi    epsilon
--   exp   = fromRational . F.exp   epsilon . toRational
--   log   = fromRational . F.log   epsilon . toRational
--   sin   = fromRational . F.sin   epsilon . toRational
--   cos   = fromRational . F.cos   epsilon . toRational
--   asin  = fromRational . F.asin  epsilon . toRational
--   acos  = fromRational . F.acos  epsilon . toRational
--   atan  = fromRational . F.atan  epsilon . toRational
--   sinh  = fromRational . F.sinh  epsilon . toRational
--   cosh  = fromRational . F.cosh  epsilon . toRational
--   asinh = fromRational . F.asinh epsilon . toRational
--   acosh = fromRational . F.acosh epsilon . toRational
--   atanh = fromRational . F.atanh epsilon . toRational

instance (Integral a, Random a) => Random (Ratio a) where
  random g = (fromIntegral n % fromIntegral d, g')
    where
      n = abs r0   :: Int
      d = maxBound :: Int
      (r0,g') = random g

  randomR (lo,hi) g = (lo + (hi-lo) * r, g')
    where (r,g') = random g

instance (Num a, Num b) => Num (a,b) where
  (a1,b1) + (a2,b2) = (a1+a2, b1+b2)
  (a1,b1) * (a2,b2) = (a1*a2, b1*b2)
  abs    (a,b)  = (abs    a, abs    b)
  signum (a,b)  = (signum a, signum b)
  negate (a,b)  = (negate a, negate b)
  fromInteger x = (fromInteger x, fromInteger x)  

instance (Fractional a, Fractional b) => Fractional (a,b) where
  fromRational x = (fromRational x, fromRational x)
  (a1,b1) / (a2,b2) = (a1/a2, b1/b2)
  
ramp :: (Fractional a, Ord a) => a -> a -> a -> a -> a -> a
ramp fromX toX fromY toY t = max (min fromY toY) $ min (max fromY toY)
                             $ fromY + ((t-fromX) / (toX-fromX)) * (toY-fromY)

sampleI :: RealFrac a => Int -> (a -> b) -> [b]
sampleI i f = sampleF f [i..]

sampleR :: RealFrac a => a -> a -> (a -> b) -> [b]
sampleR t0 tn f = sampleF f [samples t0 .. samples tn - 1]

sample :: RealFrac a => (a -> b) -> [b]
sample f = sampleF f [0..]

sampleF :: RealFrac a => (a -> b) -> [Int] -> [b]
sampleF f = map (f . (fromRational frame *) . fromIntegral)

dup :: a -> (a,a)
dup x = (x,x)

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

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

lowpass1' :: (Sample -> Sample -> Sample) -> Sample -> [Sample] -> [Sample]
lowpass1' f _     []  = []
lowpass1' f hz (x:xs) = g (a*x) xs
  where
    dt = fromRational frame
    a  = (2*pi*dt*hz) / (2*pi*dt*hz + 1)
    
    g _      []  = []
    g y_1 (x:xs) = y' : g y' xs
      where
        y  = a * x + (1-a) * y_1
        y' = f x y

lowpass1 :: Sample -> [Sample] -> [Sample]
lowpass1 = lowpass1' $ \_ -> id
    
lowpass :: Sample -> [(Sample,Sample)] -> [(Sample,Sample)]
lowpass hz = (uncurry zip) . both (lowpass1 hz) . unzip

compress :: Sample -> Sample -> [(Sample,Sample)] -> [(Sample,Sample)]
compress th hi = map (both c)
  where
    c x = signum x * f (abs x)
    f x | x < th    = x
        | otherwise = th + (x - th) * inv (x - th)
    inv x = 1 / (x*k + 1)
    k = 1 / (1 - th) - 1 / (hi - th)

vol :: Sample -> [(Sample,Sample)] -> [(Sample,Sample)]
vol v = map ((dup v) *)


