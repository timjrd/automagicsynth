module PCM where

import Data.Int
import Data.Monoid
import Data.ByteString.Builder

import System.IO

import Util

toSample :: Sample -> Builder
toSample x = int16LE $ round $ (max (-1) $ min 1 x) * fromIntegral (maxBound :: Int16)

pcm :: [(Sample,Sample)] -> Builder
pcm = mconcat . map (\(l,r) -> toSample l <> toSample r)

hPutPCM :: Handle -> [(Sample,Sample)] -> IO ()
hPutPCM h samples = do
  hSetBinaryMode h True
  hSetBuffering  h (BlockBuffering Nothing)
  hPutBuilder    h $ pcm samples

putPCM :: [(Sample,Sample)] -> IO ()
putPCM = hPutPCM stdout
