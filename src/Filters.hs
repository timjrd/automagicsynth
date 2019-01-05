module Filters where

import Util

lowpass1' :: (Sample -> Sample -> Sample) -> Sample -> [Sample] -> [Sample]
lowpass1' f _     []  = []
lowpass1' f hz (x:xs) = g (a*x) xs
  where
    dt = 1 / sampleRate
    a  = (2*pi*dt*hz) / (2*pi*dt*hz + 1)
    
    g _      []  = []
    g y_1 (x:xs) = y' : g y' xs
      where
        y  = a * x + (1-a) * y_1
        y' = f x y

lowpass1 :: Sample -> [Sample] -> [Sample]
lowpass1 = lowpass1' $ \_ -> id
    
lowpass :: Sample -> [(Sample,Sample)] -> [(Sample,Sample)]
lowpass hz = (uncurry zip) . both (lowpass1 hz) . unzip

compress :: Sample -> Sample -> [(Sample,Sample)] -> [(Sample,Sample)]
compress th hi = map (both c)
  where
    c x = signum x * f (abs x)
    f x | x < th    = x
        | otherwise = th + (x - th) * inv (x - th)
    inv x = 1 / (x*k + 1)
    k = 1 / (1 - th) - 1 / (hi - th)

vol :: Sample -> [(Sample,Sample)] -> [(Sample,Sample)]
vol v = map ((dup v) *)
