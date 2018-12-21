module Player
  ( Player
  
  , Note
  , time
  , duration
  , envelope'
  , tone
  , velocity
  , pitch
  , someNote
  
  , play
  , initNotes
  
  , seqNotes
  , parNotes
  , fromPitches
  , joinPitches
  
  , purePlayer ) where

import Data.List

import Shared
import Renderer
import Wavetable
import Envelope
import Tone

data Note = Note
  { time      :: Int
  , duration  :: Number
  , envelope' :: Envelope
  , tone      :: Wavetable
  , velocity  :: Number -> Number
  , pitch     :: Number -> Number }

type Voice = Note

instance Eq Note where
  a == b = time a == time b

instance Ord Note where
  compare a b = compare (time a) (time b)

instance Show Note where
  show x = show ( time x
                , toSamples $ duration x )

someNote = Note
  { time      = 0
  , duration  = 1
  , envelope' = someEnvelope
  , tone      = someTone
  , velocity  = const 1
  , pitch     = const 440 }

play :: Player ([Note], [Voice])
play (x:xs, voices) t = ((ys, voices''), sample)
  where
    (ys, voices'') = if t < time x
      then (x:xs,   voices')
      else (  xs, x:voices')
    
    (sample, voices') = foldl' reduce ((0,0), []) voices
    
    reduce (a, xs) x
      | t <= time x + toSamples (duration x + release (envelope' x)) = (a+b, x:xs)
      | otherwise = (a, xs)
      where
        rt = fromSamples $ t - time x
        b = synth (tone x) (pitch x rt) t
          * dup (envelope (envelope' x) (duration x) rt)
          * dup (velocity x rt)

initNotes xs = (xs, [])

seqNotes []     = []
seqNotes (x:xs) = x : f (time x + toSamples (duration x)) xs
  where f t (x:xs) = x { time = t } : f (t + toSamples (duration x)) xs
        f _ [] = []

parNotes []     = []
parNotes (x:xs) = x : map f xs
  where f y = y { time = time x }

-- fuseNotes

fromPitches :: (RealFrac a) => Note -> [a] -> [Note]
fromPitches y = map $ \hz -> y { pitch = const $ realToFrac hz }

joinPitches :: (RealFrac a) => Note -> [a] -> [Note]
joinPitches y = map f . group
  where
    f ys = y { duration = duration y * fromIntegral (length ys)
             , pitch    = const $ realToFrac $ head ys }

