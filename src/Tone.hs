module Tone where

import Control.Monad.Random

import Util

data Tone = Tone
  { modulatingToneFactor       :: Int
  , modulatingToneShift        :: Sample
  , stereoModulatingToneShift  :: Sample
  , modulationIndex            :: Sample
  , stereoModulationIndexShift :: Sample }

patch = Tone
  { modulatingToneFactor       = 5
  , modulatingToneShift        = 3
  , stereoModulatingToneShift  = 1
  , modulationIndex            = 0.5
  , stereoModulationIndexShift = 0.1 }

loPatch = Tone
  { modulatingToneFactor       = 1
  , modulatingToneShift        = -9
  , stereoModulatingToneShift  = -7
  , modulationIndex            = -3
  , stereoModulationIndexShift = -1 }

hiPatch = Tone
  { modulatingToneFactor       = 6
  , modulatingToneShift        = 9
  , stereoModulatingToneShift  = 7
  , modulationIndex            = 3
  , stereoModulationIndexShift = 1 }

instance Random Tone where
  random = randomR (loPatch,hiPatch)
  
  randomR ( (Tone m1 dm1 ddm1 im1 dim1)
          , (Tone m2 dm2 ddm2 im2 dim2) ) = runRand $ do
    m   <- getRandomR (m1  , m2  )
    dm  <- getRandomR (dm1 , dm2 )
    ddm <- getRandomR (ddm1, ddm2)
    im  <- getRandomR (im1 , im2 )
    dim <- getRandomR (dim1, dim2)
    return $ Tone m dm ddm im dim

tone :: Tone -> Sample -> (Sample -> (Sample,Sample))
tone (Tone m dm ddm im dim) hz t = mix (-0.5) 0.5 left right
  where left  = fm hz (hz * fromIntegral m + dm + ddm) (im + dim) t
        right = fm hz (hz * fromIntegral m + dm - ddm) (im - dim) t

fm :: Sample -> Sample -> Sample -> (Sample -> Sample)
fm hzc hzm im t = sin $ 2*pi * hzc * t + im * sin (2*pi * hzm * t)

mix :: Sample -> Sample -> Sample -> Sample -> (Sample,Sample)
mix panA panB a b = ( (left  + center) // (left1  + center1)
                    , (right + center) // (right1 + center1) )
  where
    left    = nabs panA * a + nabs panB * b
    left1   = nabs panA + nabs panB
    right   = pabs panA * a + pabs panB * b
    right1  = pabs panA + pabs panB
    center  = (1 - abs panA) * a + (1 - abs panB) * b
    center1 = (1 - abs panA) + (1 - abs panB)
    nabs x  = abs (min 0 x)
    pabs x  = abs (max 0 x)
    a // b  = if b == 0 then 0 else a / b
