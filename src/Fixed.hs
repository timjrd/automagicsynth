{-# LANGUAGE BinaryLiterals #-}

module Fixed
  ( Fixed
  , Int32
  , toIntBits
  , fromIntBits
  , halfBits
  , left
  , right
  , fmod ) where

import Data.Int
import Data.Bits
import Data.Ratio

-- import Data.WideWord.Int128

import System.Random

newtype Fixed = Fixed Int32
  deriving (Eq, Ord)

toIntBits :: Integral a => Fixed -> a
toIntBits (Fixed x) = fromIntegral x

fromIntBits :: Integral a => a -> Fixed
fromIntBits = Fixed . fromIntegral

halfBits = 16 :: Int

wide :: Int32 -> Int64
wide = fromIntegral

unwide :: Int64 -> Int32
unwide = fromIntegral

left :: Bits a => a -> Int -> a
left = unsafeShiftL

right :: Bits a => a -> Int -> a
right = unsafeShiftR

instance Num Fixed where
  (Fixed x) + (Fixed y) = Fixed $ x + y
  (Fixed x) - (Fixed y) = Fixed $ x - y
  (Fixed x) * (Fixed y) = Fixed $ unwide $ (wide x * wide y) `right` halfBits
  abs    (Fixed x)      = Fixed $ abs x
  signum (Fixed x)      = Fixed $ signum x `left` halfBits
  negate (Fixed x)      = Fixed $ negate x
  fromInteger x         = Fixed $ fromInteger x `left` halfBits

instance Fractional Fixed where
  fromRational x        = fromInteger (numerator x) / fromInteger (denominator x)
  (Fixed x) / (Fixed y) = Fixed $ unwide $ (wide x `left` halfBits) `div` wide y

instance Real Fixed where
  toRational (Fixed x) = fromIntegral x % (2^halfBits)

instance RealFrac Fixed where
  properFraction (Fixed x) = (fromIntegral n, Fixed f)
    where
      n = x `right` halfBits
      f = x - (n `left` halfBits)

instance Bounded Fixed where
  minBound = Fixed minBound
  maxBound = Fixed maxBound

instance Enum Fixed where
  toEnum x | x > mx = er
           | x < mn = er
           | otherwise = fromIntegral x
    where
      er = error "toEnum: out of bounds"
      mn = ceiling (minBound :: Fixed)
      mx = floor   (maxBound :: Fixed)
      
  fromEnum x = floor x
  
  succ x | x == maxBound  = error "succ maxBound :: Fixed"
         | otherwise      = x + 1
         
  pred x | x == minBound  = error "pred minBound :: Fixed"
         | otherwise      = x - 1
         
  enumFrom       x1       = enumFromThenTo x1 1  maxBound
  enumFromThen   x1 x2    = enumFromThenTo x1 x2 maxBound
  enumFromTo     x1    xn = enumFromThenTo x1 1  xn
  
  enumFromThenTo x1 x2 xn = x1 : f x2
    where f x | x <= xn   = x : f (x + (x2-x1))
              | otherwise = []  

instance Show Fixed where
  show = show . fromRational . toRational

instance Floating Fixed where
--pi    = 3.14159265
  pi    = 3 + Fixed (0b0010010000111111)
  
  exp   = undefined
  log   = undefined
  
  -- Stolen from
  -- https://github.com/kennyalive/fast-sine-cosine/blob/master/src/main.cpp

  sin w = y'
    where
      x  = ((w+pi) `fmod` (2*pi)) - pi
      b  = 4/pi
      c  = -4/(pi*pi)
      p  = 0.225
      y  = b * x + c * x * abs x
      y' = p * (y * abs y - y) + y
  
  cos x = sin $ pi/2 - x
  
  asin  = undefined
  acos  = undefined
  atan  = undefined
  sinh  = undefined
  cosh  = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined

instance Random Fixed where
  random g = (r, g')
    where
      r = fromIntegral i / m
      (i, g') = randomR (0::Int64, floor m) g
      m = maxBound
      
  randomR (lo,hi) g = (r', g')
    where
      r' = lo + r * (hi-lo)
      (r, g') = random g

fmod :: RealFrac a => a -> a -> a
fmod a b = a - n*b
  where n = fromIntegral $ floor $ a / b

