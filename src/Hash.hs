module Hash
  ( Hashable
  , Hash
  , hash
  , (<#>) ) where

import Data.Int
import Data.Word
import Data.Bits

import Data.Ratio

import Data.ReinterpretCast

-- "hash64" is translated to Haskell from this Java implementation:
-- https://web.archive.org/web/20120903003157/http://www.cris.com:80/~Ttwang/tech/inthash.htm

-- public long hash64shift(long key)
-- {
--   key = (~key) + (key << 21); // key = (key << 21) - key - 1;
--   key = key ^ (key >>> 24);
--   key = (key + (key << 3)) + (key << 8); // key * 265
--   key = key ^ (key >>> 14);
--   key = (key + (key << 2)) + (key << 4); // key * 21
--   key = key ^ (key >>> 28);
--   key = key + (key << 31);
--   return key;
-- }

hash64 :: Int64 -> Int64
hash64 key = key7
  where
    key1 = (complement key) + (key `left` 21)
    key2 = key1 `xor` (key1 `right` 24)
    key3 = (key2 + (key2 `left` 3)) + (key2 `left` 8)
    key4 = key3 `xor` (key3 `right` 14)
    key5 = (key4 + (key4 `left` 2)) + (key4 `left` 4)
    key6 = key5 `xor` (key5 `right` 28)
    key7 = key6 + (key6 `left` 31)

left :: Int64 -> Int -> Int64
left x n = x `unsafeShiftL` n
      
right :: Int64 -> Int -> Int64
right x n = fromIntegral (fromIntegral x `unsafeShiftR` n :: Word64)      

type Hash = Int64

class Hashable a where
  hash :: a -> Hash

(<#>) :: (Hashable a, Hashable b) => a -> b -> Hash
a <#> b = 31 * hash a + hash b

instance Hashable Int64 where
  hash = hash64

instance Hashable Int where
  hash = hash64 . fromIntegral

instance Hashable Float where
  hash = hash64 . fromIntegral . floatToWord

instance Hashable Double where
  hash = hash64 . fromIntegral . doubleToWord

instance Hashable Integer where
  hash x = hash64 $ fromIntegral $ (x `mod` m) * signum x
    where m = fromIntegral (maxBound :: Int64)

instance (Integral a) => Hashable (Ratio a) where
  hash x = toInteger (numerator x) <#> toInteger (denominator x)
