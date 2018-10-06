-- /!\ THIS MODULE IS FOR DEAD-CODE ARCHIVING ONLY /!\
module FixedDraft where

import Data.Int
import Data.Bits
import Data.Ratio

import System.Random

newtype Fixed = Fixed Int32
  deriving (Eq, Ord)

i64 :: Integral a => a -> Int64
i64 = fromIntegral

i32 :: Integral a => a -> Int32
i32 = fromIntegral

fmod :: RealFrac a => a -> a -> a
fmod a b = a - n*b
  where n = fromIntegral $ floor $ a / b

instance Num Fixed where
  (Fixed x) + (Fixed y) = Fixed $ x + y
  (Fixed x) - (Fixed y) = Fixed $ x - y
  (Fixed x) * (Fixed y) = Fixed $ i32 $ (i64 x * i64 y) `shiftR` 16
  abs    (Fixed x)      = Fixed $ abs x
  signum (Fixed x)      = Fixed $ signum x `shiftL` 16
  negate (Fixed x)      = Fixed $ negate x
  fromInteger x         = Fixed $ i32 x `shiftL` 16

instance Fractional Fixed where
  fromRational x        = fromInteger (numerator x) / fromInteger (denominator x)
  (Fixed x) / (Fixed y) = Fixed $ i32 $ (i64 x `shiftL` 16) `div` i64 y

instance Real Fixed where
  toRational (Fixed x) = fromIntegral x % (2^16)

instance RealFrac Fixed where
  properFraction (Fixed x) = (fromIntegral n, Fixed f)
    where
      n = x `shiftR` 16
      f = x - (n `shiftL` 16)

instance Bounded Fixed where
  minBound = fromIntegral (minBound :: Int16)
  maxBound = fromIntegral (maxBound :: Int16)

instance Random Fixed where
  random g = (fromIntegral r / fromIntegral maxB, g')
    where
      (r,g') = randomR (0, maxB) g
      maxB = maxBound :: Int16
    
  randomR (lo,hi) g = (lo + r * (hi-lo), g')
    where (r,g') = random g

instance Show Fixed where
  show = show . fromRational . toRational

instance Enum Fixed where
  toEnum x
    | x > fromIntegral (maxBound :: Int16)
      || x < fromIntegral (minBound :: Int16) = error "toEnum: out of bounds"
    | otherwise = fromIntegral x
  fromEnum x = floor x
  succ x | x == maxBound = error "succ maxBound :: Fixed"
         | otherwise     = x + 1
  pred x | x == minBound = error "pred minBound :: Fixed"
         | otherwise     = x - 1
  enumFrom       x1       = enumFromThenTo x1 1  maxBound
  enumFromThen   x1 x2    = enumFromThenTo x1 x2 maxBound
  enumFromTo     x1    xn = enumFromThenTo x1 1  xn
  enumFromThenTo x1 x2 xn = x1 : f x2
    where f x | x <= xn   = x : f (x + (x2-x1))
              | otherwise = []

instance Floating Fixed where
  pi    = 3.1416
  
  exp   = undefined
  log   = undefined
  
  sin x = negate $ f $ (x `fmod` (2*pi)) - pi
    where
      f x = p * (y * (if y < 0 then -y else y) - y) + y
        where
          b =  4 / pi
          c = -4 / (pi*pi)
          p = 0.225
          y = b * x + c * x * (if x < 0 then -x else x)
  
  cos   = undefined
  asin  = undefined
  acos  = undefined
  atan  = undefined
  sinh  = undefined
  cosh  = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined
  
