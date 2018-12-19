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
import Wavetable
import Envelope
import Tone

type Player a = a -> Number -> (a, (Sample,Sample))

data Note = Note
  { time      :: Number
  , duration  :: Number
  , envelope' :: Envelope
  , tone      :: Wavetable
  , velocity  :: Number
  , pitch     :: Number }

type Voice = Note

instance Eq Note where
  a == b = time a == time b

instance Ord Note where
  compare a b = compare (time a) (time b)

instance Show Note where
  show x = show ( floor $ time     x * 1000
                , floor $ duration x * 1000 )

someNote = Note
  { time      = 0
  , duration  = 1
  , envelope' = someEnvelope
  , tone      = someTone
  , velocity  = 1
  , pitch     = 440 }

play :: Player ([Note], [Voice])
play (x:xs, voices) t = ((ys, voices''), sample)
  where
    (ys, voices'') = if t < time x
      then (x:xs,   voices')
      else (  xs, x:voices')
    
    (sample, voices') = foldl' reduce ((0,0), []) voices
    
    reduce (a, xs) x
      | t <= time x + duration x + release (envelope' x) = (a+b, x:xs)
      | otherwise = (a, xs)
      where
        b = synth (tone x) (pitch x) t
          * dup (envelope (envelope' x) (duration x) (t - time x))
          * dup (velocity x)

initNotes xs = (xs, [])

seqNotes []     = []
seqNotes (x:xs) = x : f (time x + duration x) xs
  where f t (x:xs) = x { time = t } : f (t + duration x) xs
        f _ [] = []

parNotes []     = []
parNotes (x:xs) = x : map f xs
  where f y = y { time = time x }

fromPitches :: (RealFrac a) => Note -> [a] -> [Note]
fromPitches y = map $ \hz -> y { pitch = realToFrac hz }

joinPitches :: (RealFrac a) => Note -> [a] -> [Note]
joinPitches y = map f . group
  where
    f ys = y { duration = duration y * fromIntegral (length ys)
             , pitch    = realToFrac (head ys) }

purePlayer :: (Number -> (Sample,Sample)) -> Player ()
purePlayer f _ t = ((), f t)

