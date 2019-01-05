module Play
  ( Note(Note)
  , time
  , duration
  , noteEnvelope
  , tone
  , velocity
  , pitch
  
  , play
  , initNotes
  
  , seqNotes
  , parNotes
  , fuseNotes
  , fromPitches
  , joinPitches ) where

import Data.List

import Util
import Wavetable
import Envelope

import Render

data Note = Note
  { time         :: Number
  , duration     :: Number
  , noteEnvelope :: Envelope
  , tone         :: Wavetable
  , velocity     :: Number -> Number
  , pitch        :: Number -> Number }

type Voice = Note

instance Eq Note where
  a == b = time a == time b

instance Ord Note where
  compare a b = compare (time a) (time b)

instance Show Note where
  show x = show ( floor $ time     x * 1000
                , floor $ duration x * 1000 )

play :: Player ([Note], [Voice])
play (x:xs, voices) t = ((ys, voices''), sample)
  where
    (ys, voices'') = if t < time x
      then (x:xs,   voices')
      else (  xs, x:voices')
    
    (sample, voices') = foldl' reduce ((0,0), []) voices
    
    reduce (a, xs) x
      | t <= time x + duration x + release (noteEnvelope x) = (a+b, y:xs)
      | otherwise = (a, xs)
      where
        rt = t - time x
        (tn,v) = synth (tone x) (pitch x rt) t
        y = x { tone = tn }
        b = v
          * dup (envelope (noteEnvelope x) (duration x) rt)
          * dup (velocity x rt)

initNotes xs = (xs, [])

seqNotes []     = []
seqNotes (x:xs) = x : f (time x + duration x) xs
  where f t (x:xs) = x { time = t } : f (t + duration x) xs
        f _ [] = []

parNotes []     = []
parNotes (x:xs) = x : map f xs
  where f y = y { time = time x }

fuseNotes _ [ ] = [ ]
fuseNotes _ [x] = [x]
fuseNotes k (x0:x1:xs) = x0 { pitch = newPitch } : fuseNotes k (x1:xs)
  where
    fromPitch = pitch x0 0
    toPitch   = pitch x1 0
    dt        = duration x0
    newPitch  = ramp (dt*k) dt fromPitch toPitch

fromPitches :: (RealFrac a) => Note -> [a] -> [Note]
fromPitches y = map $ \hz -> y { pitch = const $ realToFrac hz }

joinPitches :: (RealFrac a) => Note -> [a] -> [Note]
joinPitches y = map f . group
  where
    f ys = y { duration = duration y * fromIntegral (length ys)
             , pitch    = const $ realToFrac $ head ys }

           
