module Renderer where

import GHC.ByteOrder

import Data.Int
import Data.Word
import Data.ByteString

import System.IO

import Shared

type Player a = a -> Int -> (a, (Sample,Sample))

render :: Int -> Int -> Player a -> a -> (a, ByteString)
render chunk start f a = (b,bs)
  where
    t0  = chunk * start
    n   = chunk * 2 * 2

    (bs, Just (_, _, b)) = unfoldrN n build (((0,0), (0,0)), 0::Int, a)
    
    build (bytes, i, a) = Just (byte, (bytes', i+1, b))
      where
        t = t0 + (i `div` 4)
        j = i `mod` 4
        
        (b, bytes'@((l0,l1), (r0,r1))) =
          if j == 0
          then fmap (both sampleBytes) (f a t)
          else (a, bytes)
          
        byte = case j of
          0 -> l0
          1 -> l1
          2 -> r0
          3 -> r1

sampleBytes :: Sample -> (Word8,Word8)
sampleBytes w = (w80, w81)
  where
    x   = max (-1) $ min 1 w
    mx  = fromIntegral (maxBound :: Int16)
    
    i16 = round $ x * mx   :: Int16
    w16 = fromIntegral i16 :: Word16
    
    wLE = case targetByteOrder of
             LittleEndian -> w16
             BigEndian    -> byteSwap16 w16
             
    w80 = fromIntegral wLE              :: Word8
    w81 = fromIntegral (byteSwap16 wLE) :: Word8

hPutRaw :: Handle -> Int -> Player a -> a -> IO ()
hPutRaw h chunk f init = do
  hSetBinaryMode h True
  stream 0 init
  where
    stream i a = do
      let (b,bs) = render chunk i f a
      hPut h bs
      stream (i+1) b

putRaw :: Int -> Player a -> a -> IO ()
putRaw = hPutRaw stdout

purePlayer :: (Int -> (Sample,Sample)) -> Player ()
purePlayer f _ t = ((), f t)

