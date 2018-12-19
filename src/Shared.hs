module Shared
  ( module Shared
  , module Pair ) where

import Pair
import Fixed

type Number = Fixed
type Sample = Fixed

sampleRate   = 44100          :: Number
samplePeriod = 1 / sampleRate :: Number

