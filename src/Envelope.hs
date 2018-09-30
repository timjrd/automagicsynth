module Envelope where

import Util

data Envelope = Envelope
  { attack  :: Sample
  , decay   :: Sample
  , sustain :: Sample
  , release :: Sample
  , smoothness :: Sample }

flat = 0.02

withEnvelope :: Envelope -> Sample -> [(Sample,Sample)] -> [Split (Sample,Sample)]
withEnvelope e@(Envelope _ _ _ r _) dt =
  zipWith ($) (replicate (samples dt) Fst ++ replicate (samples r) Snd)
  . zipWith (*) (map dup $ sample $ envelope e dt)

-- envelope :: Envelope -> Sample -> (Sample -> Sample)
-- envelope (Envelope a d s r) dt t
--   | t < dt    = ads t
--   | otherwise = release t
--   where
--     release t = ramp dt (dt+r) (ads dt) 0 t
--     ads t = f t - f 0
--     f t = ramp 0 a 0 1 t
--         + ramp (a+flat) (a+flat+d) 1 s t

envelope :: Envelope -> Sample -> Sample -> Sample
envelope (Envelope a d s r k) dt t = 
  ( at 0  0 a      1 (sig k) t
  + at a  s (a+d)  1 (gis k) t - 1 )
  * at dt 0 (dt+r) 1 (gis k) t
    

at :: Sample -> Sample -> Sample -> Sample -> (Sample -> Sample) -> (Sample -> Sample)
at x0 y0 x1 y1 f x = y0 + (y1 - y0) * f ((x - x0) // (x1 - x0))
  where a // b = if b == 0 then 0 else a / b

gis :: Sample -> (Sample -> Sample)
gis k = (1-) . (sig k)

sig :: Sample -> (Sample -> Sample)
sig k x
  | x <= 0 = 0
  | x >= 1 = 1
  | otherwise = ky * (dy + ff (dx + k * x))
  where
    dx = 0.5 - k * 0.5
    dy = - ff dx
    ky = 1 / (dy + ff (dx + k))
        
    ff x = 0.5 + f (x - 0.5) / 2
    f    = tanh
    -- for f, see:
    -- https://en.wikipedia.org/wiki/Sigmoid_function#/media/File:Gjl-t(x).svg
