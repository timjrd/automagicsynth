module Envelope where

import Util

data Envelope = Envelope
  { attack  :: Sample
  , decay   :: Sample
  , sustain :: Sample
  , release :: Sample }

withEnvelope :: Envelope -> Sample -> [(Sample,Sample)] -> [Split (Sample,Sample)]
withEnvelope e@(Envelope _ _ _ r) dt =
  zipWith ($) (replicate (samples dt) Fst ++ replicate (samples r) Snd)
  . zipWith (*) (map dup $ sample $ envelope e dt)

envelope :: Envelope -> Sample -> (Sample -> Sample)
envelope (Envelope a d s r) dt t
  | t < dt    = ads t
  | otherwise = release t
  where
    release t = ramp dt (dt+r) (ads dt) 0 t
    ads t = f t - f 0
    f t = ramp 0 a 0 1 t
        + ramp a (a+d) 1 s t
