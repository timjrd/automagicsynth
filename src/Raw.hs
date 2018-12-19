module Raw where

import GHC.ByteOrder

import Data.Int
import Data.Word
import Data.ByteString

import System.IO

import Shared

render :: Int -> Int -> (Number -> (Sample,Sample)) -> ByteString
render chunk start f = fst $ unfoldrN n build init
  where
    t0   = fromIntegral (start * chunk) * samplePeriod
    n    = chunk * 2 * 2
    init = (((0,0), (0,0)), 0::Int)
    
    build (bytes, i) = Just (byte, (bytes', i+1))
      where
        t = t0 + fromIntegral (i `div` 4) * samplePeriod
        j = i `mod` 4
        
        bytes'@((l0,l1), (r0,r1)) =
          if j == 0
          then both sampleBytes $ f t
          else bytes
          
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

hPutRaw :: Handle -> Int -> (Number -> (Sample,Sample)) -> IO ()
hPutRaw h chunk f = do
  hSetBinaryMode h True
  mapM_ putChunk [0..]
  where
    putChunk start = hPut h $ render chunk start f

putRaw :: Int -> (Number -> (Sample,Sample)) -> IO ()
putRaw = hPutRaw stdout
