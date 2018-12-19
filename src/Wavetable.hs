module Wavetable where

import qualified Data.Vector.Unboxed as U

import Pair
import Fixed

type Unboxed = Int64
box   = fromIntBits
unbox = toIntBits

data Wavetable = Wavetable Int (U.Vector Unboxed)
