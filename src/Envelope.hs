module Envelope where

import Shared

data Envelope = Envelope
  { attack  :: Number
  , decay   :: Number
  , sustain :: Number
  , release :: Number }

someEnvelope = Envelope 0.01 0.02 0.5 0.1

envelope :: Envelope -> Number -> (Number -> Sample)
envelope (Envelope a d s r) dt t
  | t < dt    = ads t
  | otherwise = release t
  where
    release t = ramp dt (dt+r) (ads dt) 0 t
    ads t = f t - f 0
    f t = ramp 0 a 0 1 t
        + ramp a (a+d) 1 s t
